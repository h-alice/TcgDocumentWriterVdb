{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

-- |
-- Module      : Weaviate.GraphQL
-- Description : Constructs GraphQL query strings and request bodies for Weaviate.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : MIT
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides functions to generate a specific Weaviate GraphQL
-- hybrid search query string and to structure the overall GraphQL request payload
-- in the JSON format expected by GraphQL servers over HTTP.
--
-- == Limitations
--
-- * __Specific Query__: This module currently generates only *one* type of query:
--     a hybrid search querying the `content` and `summary` properties, and
--     retrieving `content`, `summary`, and `_additional { score, explainScore, id }`.
-- * __Limit Injection__: The `limit` parameter is injected directly into the
--     query string using 'Text.Printf.printf' rather than passed as a GraphQL variable.
--     (See documentation for 'graphQLQueryTemplate' for rationale).
--
-- If more flexibility (different query types, fields, properties) is required,
-- this module would need extension or replacement, potentially using a dedicated
-- GraphQL client library.
--
-- == Example Usage
--
-- > import Weaviate.Query (HybridQuery(..), mkHybridQuery, setUserPrompt) -- From your other module
-- > import Data.Aeson (encode)
-- > import qualified Data.Text as T
-- > import qualified Data.Aeson as A
-- >
-- > -- 1. Create query parameters using Weaviate.Query
-- > let query =  WsetLimit 2 $ setAlpha 0.5 $ setUserPrompt "Some prompt." $ hyQueryCollection "SomeCollection"
-- >
-- > -- 2. Generate the GraphQL query string
-- > let queryString = mkGraphQLQueryCL (hqCollection queryParams) (hqLimit queryParams)
-- >
-- > -- 3. Define variables for the query
-- > let variables = A.object [ "q" .= hqQuery queryParams
-- >                          , "a" .= hqAlpha queryParams
-- >                          -- Note: Limit is NOT included here as it's baked into the string
-- >                          ]
-- >
-- > -- 4. Construct the full GraphQL request object
-- > let gqlRequest = GraphQLRequest {
-- >       gqlQuery = queryString,
-- >       gqlVariables = Just variables,
-- >       gqlOperationName = Just "HybridSearch" -- Must match name in query template, for now.
-- >   }
-- >
-- > -- 5. Encode the request to JSON (e.g., for sending via HTTP)
-- > let jsonPayload = encode gqlRequest
-- >
-- > -- jsonPayload now contains the JSON to send to Weaviate's /v1/graphql endpoint
-- > print jsonPayload
--
module Weaviate.GraphQL (
    -- * GraphQL Query Construction
    mkGraphQLQueryCL,
    graphQLQueryTemplate,
    -- * GraphQL Request Body
    GraphQLRequest(..),
    -- * Encoding Note
    -- | Use 'Data.Aeson.encode' from the @aeson@ library to serialize
    --   a 'GraphQLRequest' into a JSON 'BL.ByteString' suitable for HTTP requests.
) where

import qualified Data.Aeson           as A
import qualified Data.Text            as T (Text, unpack)
import Text.Printf                  (printf) -- Used for string formatting
import Data.Aeson                   (Value, object, (.=), ToJSON(toJSON))
import Data.Maybe                   (fromMaybe)


-- | A template string for a specific Weaviate GraphQL hybrid search query.
--
--   Includes placeholders handled by 'mkGraphQLQueryCL':
--     * `%s`: Collection name (e.g., "Document").
--     * `%d`: Result limit (e.g., 10).
--
--   It uses GraphQL variables `$q` (String!) for the query text and `$a` (Float!)
--   for the alpha weighting factor, which should be provided in the 'gqlVariables'
--   field of the 'GraphQLRequest'.
--
--   __Important Note on Limit__: The `limit` parameter is interpolated directly
--   into the string via `printf` (`%d`) instead of using a GraphQL variable (e.g., `$limit: Int!`). 
--   This approach was chosen due to observed issues with Weaviate's handling of integer variables in this specific context.
--   This deviates from standard GraphQL practice where all dynamic values are typically passed as variables.
--
--   __Hardcoded Fields__: This query specifically searches the `summary` and `content`
--   properties and retrieves `content`, `summary`, and standard `_additional` fields.
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
\       limit: %d \
\     ) { \
\        content \
\        summary \
\        _additional { score explainScore id } \
\     } \
\   } \
\ }"

-- | Constructs the final GraphQL query string by substituting the collection name
--   and result limit into the 'graphQLQueryTemplate' using 'printf'.
--
--   Example: `mkGraphQLQueryCL "MyDocs" 5` would produce the query string
--            with `MyDocs` and `limit: 5` inserted.
mkGraphQLQueryCL :: T.Text   -- ^ The name of the Weaviate collection.
                 -> Int    -- ^ The desired result limit (must be non-negative).
                 -> String -- ^ The fully formed GraphQL query string.
mkGraphQLQueryCL collectionName limit = printf graphQLQueryTemplate (T.unpack collectionName) limit


-- | Represents the standard structure of a GraphQL request payload,
--   typically sent as JSON over HTTP.
data GraphQLRequest = GraphQLRequest {
    gqlQuery         :: String     -- ^ The GraphQL query string (e.g., generated by 'mkGraphQLQueryCL'). REQUIRED.
  , gqlVariables     :: Maybe Value  -- ^ A JSON object containing values for variables defined in the query (e.g., $q, $a). Optional (maps to JSON `null` if `Nothing`).
  , gqlOperationName :: Maybe T.Text   -- ^ The name of the operation to execute, if the query string contains multiple named operations. Optional (maps to JSON `null` if `Nothing`). Should match a name present in 'gqlQuery' if provided.
} deriving (Show, Eq) -- Added Eq deriving, often useful.

-- | Converts a 'GraphQLRequest' into a JSON 'Value' suitable for 'A.encode'.
--   Follows the standard GraphQL HTTP request structure.
instance ToJSON GraphQLRequest where
    toJSON :: GraphQLRequest -> Value
    toJSON req = object [
        -- Key "query": The GraphQL query string itself.
        "query"         .= gqlQuery req,
        -- Key "variables": A JSON object mapping variable names (without '$') to values.
        -- If gqlVariables is Nothing, it becomes JSON null.
        "variables"     .= fromMaybe A.Null (gqlVariables req),
        -- Key "operationName": The name of the operation to run.
        -- If gqlOperationName is Nothing, it becomes JSON null.
        "operationName" .= case gqlOperationName req of
                                Nothing -> A.Null
                                Just opName -> A.String opName
        ]