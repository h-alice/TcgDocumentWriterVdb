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
-- The server listens for POST requests on the '/retrieval' endpoint and
-- GET requests on '/health'.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-} -- Allows extracting fields by name

module Main (main) where

-- Standard Libraries
import Control.Monad.IO.Class (liftIO) -- To lift IO actions within WAI Application monad
import Data.List (sortBy)
import Data.Ord (comparing)

-- OS related libraries
import System.IO (hPutStrLn, stderr) -- For printing errors/warnings
import System.Exit (exitFailure)      -- For exiting the program
import System.Environment (lookupEnv) -- For reading configuration

-- Text and Bytestring
import qualified Data.Text as T
import Text.Read (readMaybe)          -- For safely reading the port

-- Web Server Core (WAI & Warp)
import Network.Wai ( Application, pathInfo, requestMethod, responseLBS, lazyRequestBody )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types ( status200, status400, status404, status418 )
import Network.HTTP.Types.Header ( hContentType )

-- JSON Handling
import Data.Aeson ((.=))
import qualified Data.Aeson as A

-- Internal Project Modules
import Core
import qualified Weaviate.Query as WQ -- Qualified import for clarity
import qualified Weaviate.Client as WC -- Qualified import
import qualified Reranker.Client as RC -- Qualified import


-- ========================================================================== --
-- Configuration Constants & Loading                                          --
-- ========================================================================== --

data Config = Config
    { weaviateEndpoint  :: String -- ^ Weaviate endpoint URL
    , rerankerEndpoint  :: String -- ^ Reranker service endpoint URL
    , serverAddr        :: String -- ^ Server address (reserved for future use)
    , serverPort        :: Int    -- ^ Port for the web server
    }

-- Constants
-- Define Environment Variable Keys

defaultServicePort :: Int
defaultServicePort = 3000


-- | Helper to load a REQUIRED String environment variable.
--   Prints an error to stderr and exits if the variable is not set.
loadEnvRequired :: String       -- ^ Environment variable name
                -> IO String    -- ^ Loaded value (program exits if not found)
loadEnvRequired envVarName = do
    maybeValue <- lookupEnv envVarName
    case maybeValue of
        Just val -> return val  -- Value found, return it
        Nothing -> do
            -- Value not found, print error to stderr and exit
            hPutStrLn stderr $     "[Config Error] Unable to load required environment variable: '" ++ envVarName
            exitFailure -- Terminate the program

-- | Helper to load an environment variable that needs to be read into a type
--   (like Int for Port), printing warnings to stderr and using a default value if
--   the variable is not set or cannot be parsed.
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
            hPutStrLn stderr     $ "[Config Warning] Environment variable '" ++ envVarName
                                ++ "' not set. Using default value: " ++ show defaultValue
            return defaultValue

-- | Loads configuration from environment variables.
loadConfig :: IO Config -- Port type from Warp
loadConfig = do
    hPutStrLn stderr "[Config] Loading configuration from environment variables..." -- Info message

    -- Load endpoints
    wvEndpoint <- loadEnvRequired "WEAVIATE_ENDPOINT"
    rrEndpoint <- loadEnvRequired "RERANKER_ENDPOINT"

    -- Load port
    port       <- loadEnvWithFallback "SERVER_PORT" defaultServicePort

    hPutStrLn stderr "[Config] Configuration loaded successfully." -- Info message on success
    return (Config    { weaviateEndpoint = wvEndpoint,
                        rerankerEndpoint = rrEndpoint,
                        serverAddr = "0.0.0.0",
                        serverPort = port })

-- | Fetch documents from Weaviate and rerank them using the Reranker service.
-- parameters:
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

hdleRequest :: Config -> Application
hdleRequest conf request respond = do
    -- 1. Read the request body
    body <- lazyRequestBody request
    -- 2. Try to decode the JSON body into our RetrievalRequest type
    case A.eitherDecode body :: Either String RetrievalRequest of
        -- 3a. Handle JSON decoding errors
        Left err -> do
            putStrLn $ "JSON decoding failed: " ++ err
            respond $ responseLBS status400 [(hContentType, "application/json")]
                        (A.encode $ A.object ["error" .= ("Invalid JSON request: " ++ err)])
        -- 3b. Process valid requests
        Right RetrievalRequest{..} -> do
            putStrLn $ "Processing valid request ID: " ++ T.unpack reqId
            -- 4. Call the (placeholder) business logic

            -- Use liftIO as fetchDocuments is IO
            retrievedDocs <- liftIO $ fetchDocuments conf reqCollection reqQuery reqQueryParams
            case retrievedDocs of
                -- 4a. Handle errors from fetchDocuments
                Left err -> do
                    putStrLn $  "Error fetching documents for request ID: " ++ T.unpack reqId ++
                                ", error: " ++ err
                    respond $ responseLBS status400 [(hContentType, "application/json")]
                                (A.encode $ A.object ["error" .= ("Error fetching documents: " ++ err)])
                -- 4b. Successfully retrieved documents
                Right docs -> do
                    putStrLn $ "Successfully retrieved documents for request ID: " ++ T.unpack reqId
                    -- 5. Construct the successful response
                    let responsePayload = RetrievalResponse
                            { respId      = reqId -- Echo the request ID
                            , respDocuments = docs
                            }

                    -- 6. Send the JSON response
                    respond $ responseLBS status200 [(hContentType, "application/json")]
                                (A.encode responsePayload)

-- | The main WAI application. Handles routing and request processing.
app :: Config -> Application
app config request respond = do
    putStrLn $ "Received request: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
    case (requestMethod request, pathInfo request) of
        -- Handle POST requests to /retrieval
        ("POST", ["retrieval"]) -> hdleRequest config request respond
        -- Handle GET requests to /health
        ("GET", ["health"]) -> do
            putStrLn "Health check request received."
            respond $ responseLBS status200 [(hContentType, "text/plain")] "OK"
        (_, []) -> do
            putStrLn "Received root request, responding with 200 OK."
            respond $ responseLBS status200 [(hContentType, "text/plain")] "Request OK, but no specific endpoint matched. Please refer to the documentation for more details."
        (_, ["teapot"]) -> do
            putStrLn "Received a teapot request, responding with 418 I'm a teapot."
            respond $ responseLBS status418 [(hContentType, "text/plain")] "418 I'm a teapot"
        -- Handle anything else with 404 Not Found
        _ -> respond $ responseLBS status404 [(hContentType, "text/plain")] "Not Found"

main :: IO ()
main = do
    -- Load configuration (endpoints, port) - now with warnings
    conf <- loadConfig
    putStrLn   "Configuration loaded:"
    putStrLn $ "  Weaviate Endpoint: " ++ weaviateEndpoint conf
    putStrLn $ "  Reranker Endpoint: " ++ rerankerEndpoint conf
    putStrLn $ "  Service Port:      " ++ show (serverPort conf)

    putStrLn $ "Document retrieval service starting on port " ++ show (serverPort conf) ++ "..."
    -- Run the Warp server, passing configuration to the main application logic
    run (serverPort conf) (app conf)
