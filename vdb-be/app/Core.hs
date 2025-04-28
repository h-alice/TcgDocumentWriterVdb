{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE RecordWildCards   #-} -- Optional, can make response construction cleaner

module Core (
    RetrievalRequest(..),
    RetrievalResponse(..)
) where

import GHC.Generics (Generic)
import Control.Monad.IO.Class (liftIO) -- To lift IO actions into the WAI monad implicitly used

-- Web Server Core
import Network.Wai ( Application, Request, Response, pathInfo, requestMethod, responseLBS, lazyRequestBody )
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types ( Status, status200, status404, status400, status405, hContentType )

-- JSON Handling
import Data.Aeson ( FromJSON(..), ToJSON(..), genericParseJSON, genericToJSON, eitherDecode', encode, object, (.=), (.:), withObject, Options(..) )
import qualified Data.Aeson as Aeson

-- Text and Bytestring
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS (pack) -- For packing Content-Type header value


-- ========================================================================== --
-- Data Types for Request and Response                                        --
-- ========================================================================== --

-- | Represents the expected JSON structure of the incoming request.
data RetrievalRequest = RetrievalRequest
    { reqId  :: !Text            -- ^ User-provided request identifier. JSON key: "requestId".
    , reqCollection :: !Text     -- ^ The collection name to search in. JSON key: "collection".
    , reqQuery :: !Text          -- ^ The user's query string. JSON key: "query".
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
    parseJSON = withObject "RetrievalRequest" $ \v -> RetrievalRequest
        <$> v .: "requestId"
        <*> v .: "collection"
        <*> v .: "query"


-- | Deserialize RetrievalResponse from JSON.
instance ToJSON RetrievalResponse where
    toJSON RetrievalResponse{..} = object
        [ "requestId" .= respId
        , "documents" .= respDocuments
        ]

