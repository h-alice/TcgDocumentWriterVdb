-- |
-- Module      : Main
-- Description : Main executable for the Document Retrieval and Reranking Service.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a web server that exposes an API endpoint for
-- retrieving and reranking documents based on a user query. It integrates
-- with a Weaviate instance for initial retrieval and a Reranker service
-- for refining the order of results.
--
-- The server loads configuration (endpoints, port) from environment variables
-- at startup, exiting if required variables are missing. It then listens for
-- POST requests on the '/retrieval' endpoint and GET requests on '/health'.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-} -- Allows extracting fields by name

module Main (main) where

-- Standard Libraries
import Control.Monad.IO.Class (liftIO) -- To lift IO actions within WAI Application monad
import Data.List (sortBy)
import Data.Ord (comparing)
import Text.Read (readMaybe)          -- For safely reading the port

-- OS related libraries
import System.IO (hPutStrLn, stderr) -- For printing errors/warnings
import System.Exit (exitFailure)      -- For exiting the program
import System.Environment (lookupEnv) -- For reading configuration

-- Text and Bytestring
import qualified Data.Text as T

-- Web Server Core (WAI & Warp)
import Network.Wai ( Application, pathInfo, requestMethod, responseLBS, lazyRequestBody )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types ( status200, status400, status404, status405, status418, status500 )
import Network.HTTP.Types.Header ( hContentType )

-- JSON Handling
import Data.Aeson ((.=))
import qualified Data.Aeson as A

-- Internal Project Modules
import Core
    (   RetrievalResponse(RetrievalResponse, respDocuments, respId),
        RetrievalRequest(..),
        RetrievalParameters(paramTopK, paramAlpha, paramPoolSize) ) 

import qualified Weaviate.Query  as WQ   -- Qualified Weaviate Query Builder
import qualified Weaviate.Client as WC   -- Qualified Weaviate Client
import qualified Reranker.Client as RC   -- Qualified Reranker Client


-- ========================================================================== --
-- Configuration Constants & Loading                                          --
-- ========================================================================== --

-- | Holds the application's configuration, loaded from environment variables.
data Config = Config
    { weaviateEndpoint  :: !String -- ^ Weaviate endpoint URL (Loaded from WEAVIATE_ENDPOINT). Required.
    , rerankerEndpoint  :: !String -- ^ Reranker service endpoint URL (Loaded from RERANKER_ENDPOINT). Required.
    , serverAddr        :: !String -- ^ Server address to bind to (Currently hardcoded "0.0.0.0", reserved for future config).
    , serverPort        :: !Int    -- ^ Port for the web server (Loaded from SERVER_PORT, defaults to 3000).
    } deriving (Show) -- Show instance for printing config during startup


-- | Default port used if SERVER_PORT environment variable is not set or invalid.
defaultServicePort :: Int
defaultServicePort = 3000


-- | Helper to load a REQUIRED String environment variable.
--   Prints an error message to stderr and terminates the application
--   using 'exitFailure' if the environment variable is not set.
--
--   Parameters:
--     envVarName - The name of the environment variable to load.
--
--   Returns:
--     'IO String' containing the value if found. Program exits otherwise.
loadEnvRequired :: String       -- ^ Environment variable name
                -> IO String    -- ^ Loaded value (program exits if not found)
loadEnvRequired envVarName = do
    maybeValue <- lookupEnv envVarName
    case maybeValue of
        Just val -> return val  -- Value found, return it
        Nothing -> do
            -- Value not found, print error to stderr and exit
            hPutStrLn stderr $ "[Config Error] Unable to load required environment variable: '" ++ envVarName
            exitFailure -- Terminate the program

-- | Helper to load an environment variable that needs to be parsed ('Read' instance).
--   If the variable is not set or fails to parse, it prints a warning message
--   to stderr and returns the provided default value.
--
--   Parameters:
--     envVarName   - The name of the environment variable.
--     defaultValue - The value to return if the variable is missing or invalid.
--
--   Type Constraints:
--     'Read a'     - The target type must have a 'Read' instance for parsing.
--     'Show a'     - The target type must have a 'Show' instance for error messages.
--
--   Returns:
--     'IO a' containing the parsed value or the default value.
loadEnvWithFallback ::  Read a => Show a
                        => String -- ^ Environment variable name
                        -> a      -- ^ Default value
                        -> IO a   -- ^ Loaded value (either from env or default)
loadEnvWithFallback envVarName defaultValue = do
    maybeValueStr <- lookupEnv envVarName
    case maybeValueStr of
        Just valStr ->
            -- Variable found, try to parse it
            case readMaybe valStr of
                Just val -> return val -- Successfully parsed
                Nothing  -> do
                    -- Parsing failed, print warning to stderr and use default
                    hPutStrLn stderr $     "[Config Warning] Failed to parse environment variable '" ++ envVarName
                                        ++ "' value: \"" ++ valStr ++ "\". Using default value: " ++ show defaultValue
                    return defaultValue
        Nothing -> do
            -- Variable not found, print warning to stderr and use default
            hPutStrLn stderr $ "[Config Warning] Environment variable '" ++ envVarName
                            ++ "' not set. Using default value: " ++ show defaultValue
            return defaultValue

-- | Loads all necessary configuration from environment variables into a 'Config' record.
--   Uses helpers 'loadEnvRequired' (exits on failure) and 'loadEnvWithFallback'.
--
--   Returns:
--     'IO Config' containing the loaded configuration. Program exits if required variables are missing.
loadConfig :: IO Config -- Port type from Warp
loadConfig = do
    hPutStrLn stderr "[Config] Loading configuration from environment variables..." -- Info message

    -- Load endpoints
    wvEndpoint <- loadEnvRequired "WEAVIATE_ENDPOINT"
    rrEndpoint <- loadEnvRequired "RERANKER_ENDPOINT"

    -- Load port
    port       <- loadEnvWithFallback "SERVER_PORT" defaultServicePort

    hPutStrLn stderr "[Config] Configuration loaded successfully." -- Info message on success
    return (Config    
        {   weaviateEndpoint = wvEndpoint,
            rerankerEndpoint = rrEndpoint,
            serverAddr = "0.0.0.0",
            serverPort = port })

-- ========================================================================== --
-- Core Retrieval and Reranking Logic                                         --
-- ========================================================================== --

-- | Fetches documents from Weaviate based on a hybrid query, then reranks
--   the results using a Reranker service.
--
--   Parameters:
--     conf       - The application 'Config' containing endpoint URLs.
--     collection - The Weaviate collection name to query (from request).
--     query      - The user's search query text (from request).
--     params     - Additional retrieval parameters like topK, poolSize, alpha (from request).
--
--   Returns:
--     'IO (Either String [Text])' - An IO action yielding either an error message ('Left') detailing
--                                   the failure at any step (Weaviate query, parsing, Reranking),
--                                   or the list of reranked document contents ('Right').
fetchDocuments :: Config -> T.Text -> T.Text -> RetrievalParameters -> IO (Either String [T.Text])
fetchDocuments conf collection query params = do
    let wvQuery =   WQ.mkHybridQuery collection
                >>= WQ.setLimit (paramPoolSize params)
                >>= WQ.setAlpha (paramAlpha params)
                >>= WQ.setUserPrompt query

    searchResult <- WC.weaviateHybridSearch (weaviateEndpoint conf) wvQuery
    case WC.getDocuments searchResult of
        Left err -> return $ Left ("Error fetching documents: " ++ err)
        Right decodedResponse -> do
            -- Reranker
            rerankRes <- RC.rerankDocuments (rerankerEndpoint conf) query 1 decodedResponse
            case rerankRes of
                Left err -> return $ Left ("Error reranking documents: " ++ err)
                Right rerankedResults -> do
                    return $ Right $
                        take maxDoc $ -- Take the top K documents -- Take the top K documents -- Take the top K documents
                            map       -- Apply morphism to list
                            fst       -- Morphism: Choose first element of tuple
                            (sortBy   -- Sort by relevance score
                                (comparing $              -- Combinator, apply the following morphism to elements in list
                                    negate .              -- Negate to sort in descending order 
                                    RC.rrRelevanceScore . -- Take specified element of object.
                                    snd)                  -- Take second element of tuple
                            (zip decodedResponse $ RC.rrpResults rerankedResults)) -- Pair documents with rerank results
                    where
                        maxDoc = min (paramTopK params) $ length decodedResponse   -- Ensure we don't exceed the number of documents


-- ========================================================================== --
-- WAI Application Logic                                                      --
-- ========================================================================== --

-- | Handles the logic specific to the '/retrieval' endpoint (POST requests).
--   It parses the 'RetrievalRequest' from the request body, calls the
--   'fetchDocuments' function using the application 'Config', handles potential
--   errors from the fetching/reranking process, and formats the JSON response.
--
--   Error Handling:
--     - 400 Bad Request: If the incoming JSON is malformed or cannot be parsed.
--     - 500 Internal Server Error: If 'fetchDocuments' fails (e.g., issues contacting Weaviate or Reranker).
--   Success Response:
--     - 200 OK: With a JSON body containing 'requestId' and 'documents'.
--
--   Parameters:
--     conf    - The application 'Config'.
--     request - The incoming WAI 'Request'.
--     respond - The WAI function to send the 'Response'.
hdleRequest :: Config -> Application

hdleRequest conf request respond = do

    -- 1. Read the lazy request body. It's lazy, so IO happens when consumed.
    body <- lazyRequestBody request

    -- 2. Try to decode the JSON body into our RetrievalRequest type
    case A.eitherDecode body :: Either String RetrievalRequest of
        -- 3a. Handle JSON decoding errors
        Left err -> do
            putStrLn $ "[handleRetrieval] JSON decoding failed: " ++ err
            respond $ responseLBS status400 [(hContentType, "application/json")]
                        (A.encode $ A.object ["error" .= ("Invalid JSON request: " ++ err)])
        -- 3b. Process valid requests
        Right RetrievalRequest{..} -> do
            putStrLn $ "[handleRetrieval] Processing valid request ID: " ++ T.unpack reqId

            -- 4. Call the core fetching and reranking logic function.
            --    Pass the loaded config and request parameters.
            retrievedDocs <- liftIO $ fetchDocuments conf reqCollection reqQuery reqQueryParams
            case retrievedDocs of
                -- 4a. Handle errors from fetchDocuments
                Left err -> do
                    putStrLn $  "[handleRetrieval] Error fetching documents for request ID: " ++ T.unpack reqId ++ ", error: " ++ err
                    respond $ responseLBS status500 [(hContentType, "application/json")]
                                (A.encode $ A.object ["error" .= ("Error fetching documents: " ++ err)])
                -- 4b. Successfully retrieved documents
                Right docs -> do
                    putStrLn $ "Successfully retrieved documents for request ID: " ++ T.unpack reqId
                    -- 5. Construct the successful response payload
                    let responsePayload = RetrievalResponse
                            { respId      = reqId -- Echo the request ID
                            , respDocuments = docs
                            }

                    -- 6. Send the JSON response. 'encode' creates a lazy bytestring.
                    respond $ responseLBS status200 [(hContentType, "application/json")]
                                (A.encode responsePayload)

-- | The main WAI application. Acts as a router based on HTTP method and path info.
--   It passes the loaded 'Config' down to specific request handlers like 'hdleRequest'.
--
--   Routes:
--     - POST /retrieval : Handled by 'hdleRequest'.
--     - GET  /health    : Simple 200 OK response.
--     - GET  /          : Informational message.
--     - ANY  /teapot    : 418 I'm a teapot.
--     - Other           : 404 Not Found or 405 Method Not Allowed.
--
--   Parameters:
--     config  - The loaded application 'Config'.
--     request - The incoming WAI 'Request'.
--     respond - The WAI function to send the 'Response'.
app :: Config      -- ^ Application configuration
    -> Application -- ^ Resulting WAI Application
app config request respond = do
    -- Log incoming request details (optional but helpful for debugging)
    liftIO $ putStrLn $ "[app] Request received: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
    -- Route based on method and path segments
    case (requestMethod request, pathInfo request) of
        -- Handle POST requests to /retrieval, passing config to the handler
        ("POST", ["retrieval"]) -> hdleRequest config request respond

        -- Simple health check endpoint
        ("GET", ["health"]) -> do
            liftIO $ putStrLn "[app] Health check request received."
            respond $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] "OK" -- Specify charset

        -- Optional: Respond nicely to root path requests
        (_, []) -> do
            liftIO $ putStrLn "[app] Root path request received."
            respond $ responseLBS status200 [(hContentType, "text/plain; charset=utf-8")] "Service is running. Use POST /retrieval for document retrieval." -- Specify charset

        -- Optional: Fun teapot endpoint
        (_, ["teapot"]) -> do
            liftIO $ putStrLn "[app] Teapot request received."
            respond $ responseLBS status418 [(hContentType, "text/plain; charset=utf-8")] "418 I'm a teapot" -- Specify charset

        -- Handle method not allowed for known paths with wrong method
        (_, ["retrieval"]) -> respond $ responseLBS status405 [(hContentType, "text/plain; charset=utf-8")] "Method Not Allowed (POST required for /retrieval)"
        (_, ["health"])    -> respond $ responseLBS status405 [(hContentType, "text/plain; charset=utf-8")] "Method Not Allowed (GET required for /health)"

        -- Handle anything else with 404 Not Found
        _ -> respond $ responseLBS status404 [(hContentType, "text/plain; charset=utf-8")] "Not Found"


-- ========================================================================== --
-- Main Entry Point                                                           --
-- ========================================================================== --

-- | Main application entry point.
--   1. Loads configuration from environment variables ('loadConfig'). Exits if required variables are missing.
--   2. Prints the loaded configuration.
--   3. Starts the Warp web server on the configured port, passing the configuration to the main 'app'.
main :: IO ()
main = do
    -- Load configuration
    conf <- loadConfig

    -- Print loaded config
    putStrLn   "Configuration loaded:"
    putStrLn $ "  Weaviate Endpoint: " ++ weaviateEndpoint conf
    putStrLn $ "  Reranker Endpoint: " ++ rerankerEndpoint conf
    putStrLn $ "  Server Address:    " ++ serverAddr conf -- Currently hardcoded
    putStrLn $ "  Service Port:      " ++ show (serverPort conf)

    putStrLn $ "Document retrieval service starting on port " ++ show (serverPort conf) ++ "..."
    
    -- Run the Warp server, passing configuration to the main application logic
    run (serverPort conf) (app conf)
