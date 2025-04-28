{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards   #-}

module Main (main) where

-- Optional: if using generic deriving
import qualified Data.Text               as T (Text, pack, unpack)
import qualified Data.Text.IO            as TIO

import Weaviate.Query as WQ ( mkHybridQuery, setLimit, setUserPrompt, setAlpha )

import Weaviate.Client as WC (weaviateHybridSearch, getDocuments) -- Import the function to send the query
import Reranker.Client

import Data.List (sortBy)
import Data.Ord (comparing)
import Control.Lens                 ((&), (.~), (^.))
import Data.Aeson ((.=))
import Network.Wai (Application, Request, Response, pathInfo, requestMethod, responseLBS, lazyRequestBody )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status405, status400, status404, status418)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Aeson           as A (encode, object, Value, Result(..), fromJSON, eitherDecode)

import Core (RetrievalRequest(..), RetrievalResponse(..)) -- Import the data types for request and response    

import Control.Monad.IO.Class (liftIO) -- To lift IO actions into the WAI monad implicitly used
-- Constants
-- | The API endpoint for the Weaviate vector database.
-- NOTE: Replace with flexible variable such as environment variable.
weaviateEndpoint :: String
weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust if your Weaviate runs elsewhere

-- | The API endpoint for the Reranker service.
-- NOTE: Replace with flexible variable such as environment variable.
rerankerEndpoint :: String
rerankerEndpoint = "http://127.0.0.1:8089/v1/rerank"

-- | The port on which the WAI application will run.
-- NOTE: Replace with flexible variable such as environment variable.
servicePort :: Int
servicePort = 3000 -- Port for the WAI application

fetchDocuments :: T.Text -> T.Text -> IO (Either String [T.Text])
fetchDocuments query collection = do
    let wvQuery =  WQ.mkHybridQuery collection >>= WQ.setLimit 15 >>= WQ.setAlpha 0.45 >>= WQ.setUserPrompt query
    searchResult <- WC.weaviateHybridSearch weaviateEndpoint wvQuery
    case getDocuments searchResult of
        Left err -> return $ Left ("Error fetching documents: " ++ err)
        Right decodedResponse -> do
            putStrLn "Reranking documents..."
            rerankRes <- rerankDocuments rerankerEndpoint query 1 decodedResponse
            case rerankRes of
                Left err -> return $ Left ("Error reranking documents: " ++ err)
                Right rerankedResults -> do
                    return $ Right $ map -- Apply morphism to list
                        fst     -- Morphism: Choose first element of tuple
                        (sortBy -- Sort by relevance score
                            (comparing $           -- Combinator, apply the following morphism to elements in list
                                negate .           -- Negate to sort in descending order
                                rrRelevanceScore . -- Take specified element of object.
                                snd)               -- Take second element of tuple
                        (zip decodedResponse $ rrpResults rerankedResults)) -- Pair documents with rerank results

hdleRequest :: Application
hdleRequest request respond = do
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
            retrievedDocs <- liftIO $ fetchDocuments reqQuery reqCollection -- Use liftIO as fetchDocuments is IO
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
app :: Application
app request respond = do
    putStrLn $ "Received request: " ++ show (requestMethod request) ++ " " ++ show (pathInfo request)
    case (requestMethod request, pathInfo request) of
        -- Handle POST requests to /retrieval
        ("POST", ["retrieval"]) -> hdleRequest request respond
        -- Handle GET requests to /health
        ("GET", ["health"]) -> do
            putStrLn "Health check request received."
            respond $ responseLBS status200 [(hContentType, "text/plain")] "OK"
        (_, []) -> do
            putStrLn "Received root request, responding with 200 OK."
            respond $ responseLBS status200 [(hContentType, "text/plain")] "Request OK, but no specific endpoint matched.\nPlease refer to the documentation for more details."
        (_, ["teapot"]) -> do
            putStrLn "Received a teapot request, responding with 418 I'm a teapot."
            respond $ responseLBS status418 [(hContentType, "text/plain")] "418 I'm a teapot"
        -- Handle anything else with 404 Not Found
        _ -> respond $ responseLBS status404 [(hContentType, "text/plain")] "Not Found"

-- Example Usage (main function)
main :: IO ()
main = do

    --let mainQuery = "居家辦公" :: T.Text -- Example query
--
    --putStrLn "Fetching documents from Weaviate vector database..."
    ---- Fetch documents from Weaviate
    --fetchResult <- fetchDocuments mainQuery "Document" -- Example collection name
    --case fetchResult of
    --    Left err -> putStrLn $ "Error: " ++ err
    --    Right documents -> do
    --        putStrLn "--- Retrieved Documents ---"
    --        mapM_ TIO.putStrLn documents
    --        putStrLn "End of documents."

    putStrLn $ "Document fetcher service listening on " ++ show servicePort ++ "..."
    run servicePort app

