{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}


module Weaviate.GraphQL (
    mkGraphQLQueryCL,
    GraphQLRequest(..),
    graphQLQueryTemplate
) where



import qualified Data.Aeson           as A
import Data.Text                    (Text)
import Text.Printf                  (printf)
import Data.Aeson                   (Value, object, (.=), ToJSON(toJSON))
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