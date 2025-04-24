{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

-- Optional: if using generic deriving
import qualified Data.Text               as T (Text)
import qualified Data.Text.IO            as TIO

import Weaviate.Query as WQ ( mkHybridQuery, setLimit, setUserPrompt, setAlpha )

import Weaviate.Client as WC (weaviateHybridSearch, getTopNDocuments) -- Import the function to send the query


-- Example Usage (main function)
main :: IO ()
main = do
    let mainQuery = "居家辦公" :: T.Text -- Example query

    putStrLn "Building Weaviate Hybrid Query..."

    let query =  WQ.mkHybridQuery "Document" >>= WQ.setLimit 2 >>= WQ.setAlpha 0.35 >>= WQ.setUserPrompt mainQuery
    let weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust if your Weaviate runs elsewhere
    searchResult <- WC.weaviateHybridSearch weaviateEndpoint query
    putStrLn "--- Fetched Documents ---"

    case getTopNDocuments searchResult 5 of
        Left err -> putStrLn $ "Error fetching documents: " ++ err
        Right decodedResponse -> do
            mapM_ TIO.putStrLn decodedResponse

