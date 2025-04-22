{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}


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

-- import Text.RawString.QQ         -- Optional: uncomment if using QuasiQuotes for GraphQL string

-- Define the data type for Hybrid Query parameters
data HybridQuery = HybridQuery {
    hqCollection :: Text, -- The collection name (e.g., "Document")
    hqQuery :: Text,      -- The user's search query text
    hqLimit :: Int,         -- Limit the number of results
    hqAlpha :: Double       -- Balance between vector/keyword search (0=keyword, 1=vector)
} deriving (Show, Eq)     -- Deriving Show for easy printing/debugging

-- Define default values
defaultAlpha :: Double
defaultAlpha = 0.5

defaultLimit :: Int
defaultLimit = 3 -- Changed default to 3 as requested

-- 3. Implement the query builder.

-- | Initial builder function. Requires the user query.
hybridQueryBuilder :: Text -> HybridQuery
hybridQueryBuilder collection = HybridQuery {
    hqCollection = collection,
    hqQuery = T.empty,
    hqLimit = defaultLimit,
    hqAlpha = defaultAlpha
}

-- | Set the alpha value for the hybrid query.
hybridQuerySetAlpha :: Double -> HybridQuery -> HybridQuery
hybridQuerySetAlpha newAlpha hq = hq { hqAlpha = newAlpha }

-- | Set the limit for the hybrid query.
hybridQuerySetLimit :: Int -> HybridQuery -> HybridQuery
hybridQuerySetLimit newLimit hq = hq { hqLimit = newLimit }

-- | Set the user prompt.
hybridQuerySetUserPrompt :: Text -> HybridQuery -> HybridQuery
hybridQuerySetUserPrompt newQuery hq = hq { hqQuery = newQuery }

-- 4. Function to render the GraphQL query string


-- | GraphQL query template.
-- HACK: This version avoids Weaviate graphQL integer conversion issues.
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

-- | GraphQL query string with placeholders for collection name and limit.
mkGraphQLQueryCL :: Text -> Int -> String
mkGraphQLQueryCL = printf graphQLQueryTemplate

-- 5. Define the structure for the overall GraphQL request payload
data GraphQLRequest = GraphQLRequest {
    gqlQuery         :: String,
    gqlVariables     :: Maybe Value,     -- Use Maybe Value for potential JSON null
    gqlOperationName :: Maybe Text     -- Use Maybe String for potential JSON null
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
        gqlVariables = Just $ object [
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

-- 7. Example Usage (main function)
main :: IO ()
main = do
    putStrLn "Building Weaviate Hybrid Query..."

    -- Example 1: Basic query with defaults
    let query1 = hybridQuerySetUserPrompt "生涯規劃離職" $ hybridQueryBuilder "Document" -- Collection name

    let weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust if your Weaviate runs elsewhere
--
    searchResult <- weaviateHybridSearch weaviateEndpoint query1
--
    putStrLn "\n--- Weaviate Response ---"

    case searchResult of
        Left errMsg -> putStrLn $ "Error: " ++ errMsg
        Right jsonResponse -> do
            putStrLn "Success! Received JSON response:"
            -- Encode the JSON response
            BL.putStr (A.encode jsonResponse)
            putStrLn "\n--- End Response ---"