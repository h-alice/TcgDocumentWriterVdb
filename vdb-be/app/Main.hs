{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- Optional: if using generic deriving
import qualified Data.Text               as T (Text)
import qualified Data.Text.IO            as TIO

import Weaviate.Query as WQ ( mkHybridQuery, setLimit, setUserPrompt, setAlpha )

import Weaviate.Client as WC (weaviateHybridSearch, getDocuments) -- Import the function to send the query
import Reranker.Client

import Data.List (sortBy)
import Data.Ord (comparing)


-- Constants
-- | The API endpoint for the Weaviate vector database.
-- NOTE: Replace with flexible variable such as environment variable.
weaviateEndpoint :: String
weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust if your Weaviate runs elsewhere

-- | The API endpoint for the Reranker service.
-- NOTE: Replace with flexible variable such as environment variable.
rerankerEndpoint :: String
rerankerEndpoint = "http://127.0.0.1:8089/v1/rerank"


-- Example Usage (main function)
main :: IO ()
main = do

    let mainQuery = "居家辦公" :: T.Text -- Example query

    putStrLn "Fetching documents from Weaviate vector database..."

    let query =  WQ.mkHybridQuery "Document" >>= WQ.setLimit 15 >>= WQ.setAlpha 0.45 >>= WQ.setUserPrompt mainQuery
    searchResult <- WC.weaviateHybridSearch weaviateEndpoint query
    
    putStrLn "Reranking documents..."
    case getDocuments searchResult of
        Left err -> putStrLn $ "Error fetching documents: " ++ err
        Right decodedResponse -> do
            rerankRes <- rerankDocuments rerankerEndpoint mainQuery 1 decodedResponse
            case rerankRes of
                Left err -> putStrLn $ "Error: " ++ err
                Right rerankedResults -> do
                    putStrLn "--- Reranked Results ---"
                    mapM_ (\(doc :: T.Text, _) -> TIO.putStrLn doc) docRankPair where
                        docRankPair = sortBy (comparing $ negate . rrRelevanceScore . snd) (zip decodedResponse $ rrpResults rerankedResults)

