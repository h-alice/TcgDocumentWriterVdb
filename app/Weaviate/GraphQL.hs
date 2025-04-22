module Weaviate.GraphQL (
    HybridQuery(..),
) where

import Weaviate.Query (HybridQuery(..))

import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL
import qualified Network.Wreq         as W
import qualified Data.Text            as T
import Control.Lens                 ((&), (.~), (^.)) -- For wreq operators
import Control.Exception            (try, SomeException (SomeException))
import Data.Text                    (Text, pack, unpack)
import Data.Text.Encoding           (decodeUtf8)
import Text.Printf                  (printf)
import Data.Aeson                   (Value, object, (.=), ToJSON(toJSON), FromJSON(parseJSON), withObject, (.:))
import Data.Maybe                   (fromMaybe)


-- | GraphQL query template.
-- | HACK: This version avoids Weaviate GraphQL integer conversion issues using format string.
graphQLQueryTemplate :: String
graphQLQueryTemplate = " \
\ query HybridSearch($q: String!, $a: Float!) { \
\   Get { \
\     %s ( \
\       hybrid: { \
\         query: $q, \
\         alpha: $a, \
\         properties: [\"summary\", \"content\"] \
\       }, \
\       limit: %d, \
\     ) { \
\        content \
\        summary \
\        _additional { score explainScore id } \
\     } \
\   } \
\ }"

-- | Make GraphQL query string with placeholders for collection name and limit.
-- | HACK: This version avoids Weaviate GraphQL integer conversion issues using format string.
mkGraphQLQueryCL :: Text -> Int -> String
mkGraphQLQueryCL = printf graphQLQueryTemplate

-- | Define the structure for the overall GraphQL request payload
data GraphQLRequest = GraphQLRequest {
    gqlQuery         :: String,
    gqlVariables     :: Maybe Value,   -- Use Maybe Value for potential JSON null
    gqlOperationName :: Maybe Text     -- Use Maybe Text for potential JSON null
} deriving (Show)

-- | Aeson instance to convert GraphQLRequest to the JSON format Weaviate expects.
instance ToJSON GraphQLRequest where
    toJSON :: GraphQLRequest -> Value
    toJSON req = object [
        "query"         .= gqlQuery req,
        "variables"     .= fromMaybe A.Null (gqlVariables req),    -- Use A.Null for JSON null
        "operationName" .= maybe A.Null A.String (gqlOperationName req) -- Use A.Null for JSON null
        ]

-- 6. Function to send the query using wreq

-- | Sends a hybrid search query to a Weaviate instance.
-- Returns Either an error message (String) or the parsed JSON response (Value).
weaviateHybridSearch :: String         -- ^ Weaviate GraphQL endpoint URL (e.g., "http://localhost:8080/v1/graphql")
                        -> HybridQuery    -- ^ The hybrid query parameters
                        -> IO (Either String Value) -- ^ Result wrapped in IO and Either
weaviateHybridSearch weaviateUrl hq = do
    let baseGraphqlString = mkGraphQLQueryCL (hqCollection hq) (hqLimit hq)
    let requestPayload = GraphQLRequest {
        gqlQuery = baseGraphqlString,  -- GraphQL query string built in previous step.
        gqlVariables = Just $ A.object [
            "q" .= hqQuery hq, -- User query
            "a" .= hqAlpha hq  -- Alpha value
        ],
        gqlOperationName = Just "HybridSearch" -- Optional: specify the operation name
    }

    -- Set request options: specify JSON content type
    let opts = W.defaults & W.header "Content-Type" .~ ["application/json"]

    -- Use `try` to catch potential exceptions (network errors, etc.)
    eResult <- try (W.postWith opts weaviateUrl (A.encode requestPayload)) :: IO (Either SomeException (W.Response BL.ByteString))

    case eResult of
        -- Handle network/HTTP exceptions
        Left err -> return $ Left (printf "HTTP request failed: %s" (show err))

        -- Process the successful HTTP response
        Right resp -> do
            let statusCode = resp ^. W.responseStatus . W.statusCode
            let statusMsg = resp ^. W.responseStatus . W.statusMessage
            let body = resp ^. W.responseBody

            if statusCode >= 200 && statusCode < 300
                then do
                    -- Attempt to decode the response body as JSON
                    case A.eitherDecode body of
                        Left jsonErr -> return $ Left (printf "JSON decoding failed: %s\nRaw body: %s" jsonErr (show body))
                        Right jsonValue -> return $ Right jsonValue
                else do
                    -- Handle non-2xx HTTP status codes
                    return $ Left (printf "Weaviate returned error %d: %s\nBody: %s"
                                    statusCode (unpack $ decodeUtf8 statusMsg) (show body))

