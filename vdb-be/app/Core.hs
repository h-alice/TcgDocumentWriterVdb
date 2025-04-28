{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-} -- Optional, can make response construction cleaner
{-# LANGUAGE InstanceSigs #-}

module Core (
    RetrievalRequest(..),
    RetrievalResponse(..)
) where

import GHC.Generics (Generic)

-- JSON Handling
import Data.Aeson ( FromJSON(..), ToJSON(..), object, (.=), (.:), withObject )
import qualified Data.Aeson as Aeson

-- Text and Bytestring
import Data.Text (Text)
 -- For packing Content-Type header value
import Data.Aeson ((.:?), (.!=)) -- For optional fields in JSON parsing
import Data.Aeson.Types (Parser) -- For custom parsing


-- ========================================================================== --
-- Data Types for Request and Response                                        --
-- ========================================================================== --

-- | Represents the expected JSON structure of the incoming request.
data RetrievalRequest = RetrievalRequest
    { reqId  :: !Text            -- ^ User-provided request identifier. JSON key: "requestId".
    , reqCollection :: !Text     -- ^ The collection name to search in. JSON key: "collection".
    , reqQuery :: !Text          -- ^ The user's query string. JSON key: "query".
    , reqTopK :: !Int            -- ^ The number of top results to return. JSON key: "topK".
    , reqPoolSize :: !Int        -- ^ Optional: The number of documents to retrieve. JSON key: "poolSize".
    , reqAlpha :: !Double   -- ^ Optional: The alpha parameter for hybrid search. JSON key: "alpha".
    } deriving (Show, Generic)   -- Generic needed if using generic Aeson instances

-- | Represents the JSON structure of the response to be sent.
data RetrievalResponse = RetrievalResponse
    { respId      :: !Text   -- ^ The request identifier, echoed back from the request. JSON key: "requestId".
    , respDocuments :: ![Text] -- ^ A list of retrieved document contents. JSON key: "documents".
    } deriving (Show, Generic)


-- ========================================================================== --
-- Aeson Instances for JSON Serialize and Deserialize                         --
-- ========================================================================== --

-- | RetrievalRequest from JSON.
instance FromJSON RetrievalRequest where
    parseJSON :: Aeson.Value -> Parser RetrievalRequest
    parseJSON = withObject "RetrievalRequest" $ \v -> RetrievalRequest
        <$> v .: "requestId"
        <*> v .: "collection"
        <*> v .: "query"
        <*> v .: "topK" .!= 3
        <*> v .:? "poolSize" .!= 20
        <*> v .:? "alpha" .!= 0.5

-- | Deserialize RetrievalResponse from JSON.
instance ToJSON RetrievalResponse where
    toJSON :: RetrievalResponse -> Aeson.Value
    toJSON RetrievalResponse{..} = object
        [ "requestId" .= respId
        , "documents" .= respDocuments
        ]

