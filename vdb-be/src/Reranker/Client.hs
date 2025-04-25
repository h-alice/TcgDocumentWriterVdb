{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-} -- For cleaner record construction/pattern matching
{-# LANGUAGE DeriveGeneric     #-} -- For Show, Eq derivations
{-# LANGUAGE InstanceSigs      #-} -- For explicit type sigs on instances
{-# LANGUAGE StrictData        #-} -- Good practice for performance critical fields

-- |
-- Module      : Reranker.Client
-- Description : Client for interacting with a simple reranking API endpoint.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
--  -- This module provides functionality to send requests to a reranking API
--  -- (as specified by the endpoint URL) and parse the corresponding responses.
--  -- It includes data types for the request/response payloads and a client function
--  -- using the 'wreq' library for HTTP communication.
--  --
--  -- == Example Usage
--  --
--  -- > import qualified Reranker.Client as RC
--  -- > import qualified Data.Text as T
--  -- > import Data.Either (Either(..))
--  -- >
--  -- > main :: IO ()
--  -- > main = do
--  -- >     let rerankerUrl = "http://127.0.0.1:8089/v1/rerank" -- Your reranker API endpoint
--  -- >     let userQuery = T.pack "What is the capital of France?"
--  -- >     let documentsToRerank =
--  -- >           [ T.pack "Paris is the capital of France."
--  -- >           , T.pack "Lyon is a major city in France."
--  -- >           , T.pack "The Eiffel Tower is in Paris."
--  -- >           ]
--  -- >     let numResults = 5 -- Corresponds to top_n in the request
--  -- >
--  -- >     putStrLn "Sending documents for reranking..."
--  -- >     eResponse <- RC.rerankDocuments rerankerUrl userQuery numResults documentsToRerank
--  -- >
--  -- >     case eResponse of
--  -- >       Left err -> putStrLn $ "Reranking failed: " ++ err
--  -- >       Right rerankResp -> do
--  -- >           putStrLn $ "Reranking successful using model: " ++ T.unpack (RC.rrpModel rerankResp)
--  -- >           putStrLn $ "Usage: " ++ show (RC.rrpUsage rerankResp)
--  -- >           putStrLn "Results:"
--  -- >           mapM_ print (RC.rrpResults rerankResp)
--  --
module Reranker.Client (
    -- * Client Function
    rerankDocuments,

    -- * Request Type
    RerankRequest(..),
    defaultRerankerModel, -- Export the default model name

    -- * Response Types
    RerankResponse(..),
    RerankResult(..),
    RerankUsage(..)
) where

import GHC.Generics (Generic) -- For deriving Show, Eq
import qualified Data.Aeson           as A
import qualified Data.Aeson.Types     as AT (Parser)
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq         as W
import qualified Data.Text            as T
import Control.Lens                 ((&), (.~), (^.)) -- For wreq operators
import Control.Exception            (try, SomeException)
import Data.Text.Encoding           (decodeUtf8') -- Use safe decoding for status msg
import Text.Printf                  (printf)
import Data.Aeson                   ( FromJSON(..), ToJSON(..)
                                    , Value, object, (.=), (.:)
                                    , withObject )

-- | The default model name used for reranking requests.
defaultRerankerModel :: T.Text
defaultRerankerModel = "local-reranker-model"

-- ========================================================================== --
-- Request Data Type and Instances                                            --
-- ========================================================================== --

-- | Represents the request payload sent to the reranker API.
data RerankRequest = RerankRequest
    { rrModel     :: !T.Text   -- ^ The name of the reranker model to use. JSON key: "model".
    , rrQuery     :: !T.Text   -- ^ The user query text. JSON key: "query".
    , rrTopN      :: !Int      -- ^ The number of results expected. JSON key: "top_n". NOTE: we found the client will ignore this field.
    , rrDocuments :: ![T.Text] -- ^ The list of documents (as text) to be reranked. JSON key: "documents".
    } deriving (Show, Eq, Generic)

-- | Converts 'RerankRequest' to the JSON format expected by the API.
instance ToJSON RerankRequest where
    toJSON :: RerankRequest -> Value
    toJSON RerankRequest{..} = object
        [ "model"     .= rrModel
        , "query"     .= rrQuery
        , "top_n"     .= rrTopN
        , "documents" .= rrDocuments
        ]

-- ========================================================================== --
-- Response Data Types and Instances                                          --
-- ========================================================================== --

-- Expected JSON response structure:
-- > {'model': 'dummy-reranker-model',
-- >  'object': 'list',
-- >  'usage': {'prompt_tokens': 478, 'total_tokens': 478},
-- >  'results': [{'index': 0, 'relevance_score': -0.008283790200948715},
-- >   {'index': 1, 'relevance_score': -0.06083318963646889},
-- >   {'index': 2, 'relevance_score': -0.007102716714143753}]}

-- | Represents the token usage information in the reranker response.
data RerankUsage = RerankUsage
    { ruPromptTokens :: !Int -- ^ Tokens in the prompt/input. JSON key: "prompt_tokens".
    , ruTotalTokens  :: !Int -- ^ Total tokens processed. JSON key: "total_tokens".
    } deriving (Show, Eq, Generic)

-- | Parses the "usage" object from the JSON response.
instance FromJSON RerankUsage where
    parseJSON :: Value -> AT.Parser RerankUsage
    parseJSON = withObject "RerankUsage" $ \v -> RerankUsage
        <$> v .: "prompt_tokens"
        <*> v .: "total_tokens"

-- | Represents a single reranked result, containing the original index and score.
data RerankResult = RerankResult
    { rrIndex          :: !Int    -- ^ The original 0-based index of the document in the input list. JSON key: "index".
    , rrRelevanceScore :: !Double -- ^ The relevance score assigned by the reranker model. JSON key: "relevance_score".
    } deriving (Show, Eq, Generic)

-- | Parses a single result object from the "results" array in the JSON response.
instance FromJSON RerankResult where
    parseJSON :: Value -> AT.Parser RerankResult
    parseJSON = withObject "RerankResult" $ \v -> RerankResult
        <$> v .: "index"
        <*> v .: "relevance_score"

-- | Represents the overall structure of the successful reranker API response.
data RerankResponse = RerankResponse
    { rrpModel   :: !T.Text          -- ^ The model name used for reranking. JSON key: "model".
    , rrpObject  :: !T.Text          -- ^ The type of object returned (e.g., "list"). JSON key: "object".
    , rrpUsage   :: !RerankUsage     -- ^ Token usage information. JSON key: "usage".
    , rrpResults :: ![RerankResult]  -- ^ The list of reranked results. JSON key: "results".
    } deriving (Show, Eq, Generic)

-- | Parses the top-level JSON response object.
instance FromJSON RerankResponse where
    parseJSON :: Value -> AT.Parser RerankResponse
    parseJSON = withObject "RerankResponse" $ \v -> RerankResponse
        <$> v .: "model"
        <*> v .: "object"
        <*> v .: "usage"
        <*> v .: "results"

-- ========================================================================== --
-- Client Function                                                            --
-- ========================================================================== --

-- | Sends documents and a query to the reranker API and parses the response.
--
--   Uses the 'defaultRerankerModel'. Handles network errors, non-2xx HTTP status codes,
--   and JSON parsing errors.
rerankDocuments :: String           -- ^ The full URL of the reranker API endpoint (e.g., "http://host:port/v1/rerank").
                -> T.Text           -- ^ The user query.
                -> Int              -- ^ The desired number of top results (`top_n`).
                -> [T.Text]         -- ^ The list of document contents to rerank.
                -> IO (Either String RerankResponse) -- ^ Error message or the successfully parsed 'RerankResponse'.
rerankDocuments apiUrl query topN docs = do
    -- 1. Construct the request payload
    let requestPayload = RerankRequest
            { rrModel     = defaultRerankerModel
            , rrQuery     = query
            , rrTopN      = topN
            , rrDocuments = docs
            }

    -- 2. Configure HTTP request options
    let opts =    W.defaults
                & W.header "Content-Type" .~ ["application/json"]

    -- 3. Perform the HTTP POST request, catching potential exceptions
    eResult <- try (W.postWith opts apiUrl (A.encode requestPayload))
                :: IO (Either SomeException (W.Response BL.ByteString))

    -- 4. Process the result
    case eResult of
        Left networkErr -> return $ Left (printf "HTTP request failed: %s" (show networkErr))
        Right resp -> do
            let statusCode = resp ^. W.responseStatus . W.statusCode
            let statusMsgBytes = resp ^. W.responseStatus . W.statusMessage
            let responseBody = resp ^. W.responseBody

            -- Check for successful HTTP status code (2xx)
            if statusCode >= 200 && statusCode < 300
                then -- Success: Try to parse the response body into RerankResponse
                    case A.eitherDecode responseBody of
                        Left jsonErr -> return $ Left (printf "JSON decoding of successful response failed: %s\nRaw body: %s" jsonErr (show responseBody))
                        Right rerankResp -> return $ Right rerankResp
                else -- Reranker API returned an error status
                    -- Safely decode status message for inclusion in error
                    let statusMsgText = case decodeUtf8' statusMsgBytes of
                                            Left _ -> "(non-utf8 status message)"
                                            Right t -> T.unpack t
                    in return $ Left (printf "Reranker API request failed with status %d: %s\nResponse body: %s"
                                        statusCode statusMsgText (show responseBody))