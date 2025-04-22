{-# LANGUAGE OverloadedStrings #-}

module Main (main) where
    

import Weaviate.Query as WQ (
    hyQueryCollection,
    setLimit,
    setUserPrompt,
    setAlpha
    )

import Weaviate.Client as WC (weaviateHybridSearch) -- Import the function to send the query

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL

-- Example Usage (main function)
main :: IO ()
main = do
    putStrLn "Building Weaviate Hybrid Query..."

    -- Example 1: Basic query with defaults
    let query =  WQ.setLimit 2 $ WQ.setAlpha 0.5 $ WQ.setUserPrompt "生涯規劃離職" $ WQ.hyQueryCollection "Document" -- Collection name
    putStrLn $ "Query: " ++ show query
    let weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust if your Weaviate runs elsewhere
    searchResult <- WC.weaviateHybridSearch weaviateEndpoint query
    putStrLn "\n--- Weaviate Response ---"

    case searchResult of
        Left errMsg -> putStrLn $ "Error: " ++ errMsg
        Right jsonResponse -> do
            putStrLn "Success! Received JSON response:"
            -- Encode the JSON response
            BL.putStr (A.encode jsonResponse)
            putStrLn "\n--- End Response ---"