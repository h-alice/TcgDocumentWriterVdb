-- Module      : Weaviate.Query
-- Description : Builder for Weaviate Hybrid Search GraphQL query parameters.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a data type 'HybridQuery' and a builder pattern
-- for constructing the parameters needed for a Weaviate GraphQL hybrid search query.
--
-- == Example Usage:
--
-- > import qualified Data.Text as T
-- >
-- > -- Start building a query for the "Document" collection with chain validation
-- > -- Chain validating setters using >>=
-- > let eitherParams :: Either String HybridQuery
-- >     eitherParams = mkHybridQuery (T.pack "Document")                -- Start the Either chain
-- >                    >>= setUserPrompt (T.pack "Benefits of Haskell") -- Assume setUserPrompt doesn't fail
-- >                    >>= setLimit 10      -- Apply setLimit
-- >                    >>= setAlpha 0.6     -- Apply setAlpha
-- >                    -- >>= setLimit (-5) -- This would cause a Left with an error message
module Weaviate.Query (
    -- * Hybrid Query Type
    HybridQuery(..),
    -- * Query Builder
    mkHybridQuery,
    -- * Setter Functions
    setUserPrompt,
    setLimit,
    setAlpha,
) where

import qualified Data.Text as T
import Data.Text (Text)
-- Or: import Data.Function ((&))

-- | Parameters for a Weaviate hybrid search query.
--   Use 'mkHybridQuery' and setter functions to construct values of this type.
data HybridQuery = HybridQuery {
        hqCollection :: !Text   -- ^ __Required.__ The name of the Weaviate collection.
    ,   hqQuery      :: !Text   -- ^ __Required.__ The user's natural language query prompt.
    ,   hqLimit      :: !Int    -- ^ The maximum number of results. Validated: must be non-negative.
    ,   hqAlpha      :: !Double -- ^ Weighting factor ($[0, 1]$). $0$=keyword, $1$=vector. Validated: must be in range $[0, 1]$.
} deriving (Show, Eq)

-- | Default value for the hybrid search alpha parameter ($0.5$).
defaultAlpha :: Double
defaultAlpha = 0.5

-- | Default value for the result limit ($3$).
defaultLimit :: Int
defaultLimit = 3

-- | Initial builder function. Creates a 'HybridQuery' with default values.
--   You __must__ set the user query using 'setUserPrompt'.
--   This function does not perform validation itself.
mkHybridQuery :: Text -- ^ The target collection name.
                -> Either String HybridQuery
mkHybridQuery collection = Right $ HybridQuery {
    hqCollection = collection,
    hqQuery = T.empty, -- User must set this via setUserPrompt
    hqLimit = defaultLimit,
    hqAlpha = defaultAlpha
}

-- | Set the user's query prompt. This is typically required for a meaningful search.
setUserPrompt :: Text -- ^ The user's search query.
                -> HybridQuery -> Either String HybridQuery
setUserPrompt newQuery hq = Right $ hq { hqQuery = newQuery }

-- | Set the result limit for the hybrid query, validating the input.
--   Returns 'Left' with an error message if the limit is negative.
setLimit :: Int -- ^ The desired maximum number of results ($\ge 0$).
            -> HybridQuery -> Either String HybridQuery
setLimit newLimit hq
    | newLimit >= 0 = Right $ hq { hqLimit = newLimit }
    | otherwise     = Left $ "Invalid limit: " ++ show newLimit ++ ". Limit must be non-negative."

-- | Set the alpha value (weighting factor) for the hybrid query, validating the input.
--   Returns 'Left' with an error message if alpha is outside the range $[0, 1]$.
setAlpha :: Double -- ^ The desired alpha value ($[0, 1]$).
            -> HybridQuery -> Either String HybridQuery
setAlpha newAlpha hq
    | newAlpha >= 0.0 && newAlpha <= 1.0 = Right $ hq { hqAlpha = newAlpha }
    | otherwise                          = Left $ "Invalid alpha: " ++ show newAlpha ++ ". Alpha must be between 0.0 and 1.0 (inclusive)."