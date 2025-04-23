{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-} -- Optional: for cleaner record construction/pattern matching
{-# LANGUAGE DeriveGeneric     #-} -- Optional: for generic deriving (though manual is clearer here)
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Weaviate.Response
-- Description : Defines types and FromJSON instances for parsing Weaviate GraphQL 'Get' responses.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : MIT -- Choose an appropriate license
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides data types and corresponding `FromJSON` instances
-- to parse the specific JSON structure returned by Weaviate's GraphQL API
-- for 'Get' queries. It handles the nested structure including 'data', 'Get',
-- a dynamically named collection field (e.g., "DocumentDoc"), and the '_additional'
-- metadata object within each result.
--
-- Example JSON structure handled:
-- {
--   "data": {
--     "Get": {
--       "YourCollectionName": [
--         {
--           "_additional": { ... },
--           "content": "...",
--           "summary": "..."
--         },
--         ...
--       ]
--     }
--   }
-- }
module Weaviate.Response
  ( -- * Types
    WeaviateResponse(..)
  , GetResponseData(..)
  , GetDocument(..)
  , QueryResult(..)
  , AdditionalInfo(..)
    -- * Note
    -- | The 'FromJSON' instances are defined for each type, enabling
    --   direct decoding using libraries like Aeson (e.g., 'Data.Aeson.eitherDecode').
  ) where

import Data.Aeson     (FromJSON (parseJSON), Value, eitherDecode, withObject, (.:))
import GHC.Generics   (Generic) -- Optional: if using generic deriving
import qualified Data.Text.Lazy.Encoding as LTE (encodeUtf8)
import qualified Data.Text.Lazy          as LT (pack)
import qualified Data.Aeson.Types        as AesonTypes
import qualified Data.Text               as T (Text, unpack)
import qualified Data.Aeson.KeyMap       as KM
import qualified Data.Text.IO            as TIO


-- | Represents the `_additional` metadata object within a Weaviate query result.
--   This contains information like the document ID and relevance score.
data AdditionalInfo = AdditionalInfo
    { addExplainScore :: !T.Text -- ^ Explanation of the score calculation. Corresponds to the `explainScore` JSON key.
    , addId           :: !T.Text -- ^ Unique identifier of the document. Corresponds to the `id` JSON key.
    , addScore        :: !Double -- ^ The relevance score as a numerical value. Corresponds to the `score` JSON key. Note: The `score` JSON field is expected as a string and is converted to `Double` during parsing.
    } deriving (Show, Eq, Generic) -- Add Generic if using generic FromJSON

-- | Manual `FromJSON` instance for `AdditionalInfo`.
--   Handles the direct mapping of JSON keys (`explainScore`, `id`, `score`)
--   and the conversion of the `score` field from Text to Double.
instance FromJSON AdditionalInfo where
    parseJSON :: Value -> AesonTypes.Parser AdditionalInfo
    parseJSON = withObject "AdditionalInfo" $ \v -> do
        -- Use (.:) operator to extract fields by their JSON key
        explainScore <- v .: "explainScore"
        idVal        <- v .: "id" -- Use different name to avoid clash with Prelude.id
        scoreText    <- v .: "score"
        -- Convert the score from Text to Double, defaulting to 0.0 on parse failure
        let score = case reads (T.unpack scoreText) :: [(Double, String)] of
                        [(num, "")] -> num
                        _           -> 0.0 -- Or consider failing: `fail $ "Could not parse score: " ++ T.unpack scoreText`
        -- Construct the record
        return AdditionalInfo   { addExplainScore = explainScore
                                , addId = idVal
                                , addScore = score
                                }

-- | Represents a single document result within the main results array
--   returned by a Weaviate 'Get' query.
data QueryResult = QueryResult
    { qrAdditional :: !AdditionalInfo -- ^ Contains additional metadata like ID and score. Maps to the `_additional` JSON object.
    , qrContent    :: !T.Text         -- ^ The main content of the document. Maps to the `content` JSON key.
    , qrSummary    :: !T.Text         -- ^ A summary of the document content. Maps to the `summary` JSON key.
    } deriving (Show, Eq, Generic)

-- | Manual `FromJSON` instance for `QueryResult`.
--   Uses the `FromJSON` instance for `AdditionalInfo` to parse the nested `_additional` object.
instance FromJSON QueryResult where
    parseJSON :: Value -> AesonTypes.Parser QueryResult
    parseJSON = withObject "QueryResult" $ \v -> do
        -- The FromJSON instance for AdditionalInfo handles parsing the nested object
        additional <- v .: "_additional"
        content    <- v .: "content"
        summary    <- v .: "summary"
        return QueryResult  { qrAdditional = additional
                            , qrContent = content
                            , qrSummary = summary
                            }

-- | Represents the value associated with the dynamically named collection
--   field (e.g., `"Document"`) inside the `Get` object. It primarily
--   holds the list of results.
newtype GetDocument = GetDocument
    { gdResults :: [QueryResult] -- ^ The list of individual query results (`QueryResult`).
    } deriving (Show, Eq, Generic)

-- | Custom `FromJSON` instance for `GetDocument`.
--   Parses the object containing the results array. Crucially, it expects
--   __exactly one__ field within the JSON object (e.g., `{"DocumentDoc": [...]}`).
--   It extracts the array associated with that single field, regardless of the field's name,
--   and parses it as a list of `QueryResult`.
--   Fails if the object is empty or contains more than one field.
instance FromJSON GetDocument where
    parseJSON :: Value -> AesonTypes.Parser GetDocument
    parseJSON = withObject "GetDocument" $ \v -> do
        -- Extract the first (and expected only) value from the KeyMap.
        case KM.toList v of
            [] -> fail "No collection field found inside the 'Get' object. Expected one field like 'CollectionName': [...]"
            [(_, value)] -> do
                -- Parse the value (which should be the array of results)
                results <- parseJSON value
                return GetDocument { gdResults = results }
            _ -> fail $ "Multiple fields found inside the 'Get' object: " ++ show (KM.keys v) ++ ". Expected exactly one collection field."


-- | Represents the object nested under the `"data"` key, specifically
--   containing the `"Get"` object.
newtype GetResponseData = GetResponseData
    { grdGet :: GetDocument -- ^ Holds the parsed content of the `"Get"` object (`GetDocument`). Maps to the `Get` JSON key.
    } deriving (Show, Eq, Generic)

-- | `FromJSON` instance for `GetResponseData`.
--   Extracts the object associated with the `"Get"` key.
instance FromJSON GetResponseData where
    parseJSON :: Value -> AesonTypes.Parser GetResponseData
    parseJSON = withObject "GetResponseData" $ \v -> do
        -- Extract the object associated with the key "Get"
        getObj <- v .: "Get"
        -- The FromJSON instance for GetDocument handles parsing the inner content
        return GetResponseData { grdGet = getObj }

-- | Represents the top-level structure of the Weaviate GraphQL 'Get' response.
newtype WeaviateResponse = WeaviateResponse
    { wrData :: GetResponseData -- ^ Contains the main data payload. Maps to the `data` JSON key.
    } deriving (Show, Eq, Generic)

-- | `FromJSON` instance for `WeaviateResponse`.
--   Extracts the object associated with the `"data"` key.
instance FromJSON WeaviateResponse where
    parseJSON :: Value -> AesonTypes.Parser WeaviateResponse
    parseJSON = withObject "WeaviateResponse" $ \v -> do
        -- Extract the object associated with the key "data"
        dataObj <- v .: "data"
        -- The FromJSON instance for GetResponseData handles parsing the inner content
        return WeaviateResponse { wrData = dataObj }

-- Example Usage:
-- main :: IO ()
-- main = do
--    let jsonResponse = "..." -- Your JSON string here
--    case eitherDecode jsonResponse :: Either String WeaviateResponse of
--        Left err -> putStrLn $ "Decoding failed: " ++ err
--        Right decodedResponse -> do
--            putStrLn "Decoding successful!"
--            print decodedResponse
--            let results = gdResults . grdGet . wrData $ decodedResponse
--            -- ... access results ...