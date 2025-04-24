-- Module      : Weaviate.Query
-- Description : Builder for Weaviate Hybrid Search GraphQL query parameters using Either for validation.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides a data type 'HybridQuery' and a builder pattern
-- for constructing the parameters needed for a Weaviate GraphQL hybrid search query.
--
-- Setters like 'setLimit' and 'setAlpha' now return 'Either String HybridQuery',
-- providing specific error messages in the 'Left' case if validation fails.
-- Use the '(>>=)' operator from 'Control.Monad' (or Prelude) to chain these
-- validation steps.
--
-- == Example Usage:
--
-- > import qualified Data.Text as T
-- > import Data.Either (Either(..))
-- >
-- > -- Start building a query for the "Document" collection
-- > let initialQuery = mkHybridQuery (T.pack "Document")
-- >                      & setUserPrompt (T.pack "Benefits of Haskell") -- Assume setUserPrompt doesn't fail
-- >
-- > -- Chain validating setters using >>=
-- > let eitherParams :: Either String HybridQuery
-- >     eitherParams = Right initialQuery -- Start the Either chain
-- >                    >>= setLimit 10      -- Apply setLimit
-- >                    >>= setAlpha 0.6     -- Apply setAlpha
-- >                    -- >>= setLimit (-5) -- This would cause a Left with an error message
-- >
-- > -- Handle the final result
-- > case eitherParams of
-- >   Left errMsg -> putStrLn $ "Failed to build query: " ++ errMsg
-- >   Right validQuery -> do
-- >       putStrLn $ "Successfully built query: " ++ show validQuery
-- >       -- Proceed to use validQuery with Weaviate.Client.weaviateHybridSearch
--
module Weaviate.Query (
    -- * Hybrid Query Type
    HybridQuery(..),
    -- * Query Builder
    -- | Use 'mkHybridQuery' to start, optionally chain non-validating setters like
    --   'setUserPrompt' using '&', then chain validating setters like 'setLimit'
    --   and 'setAlpha' using '(>>=)'. Handle the final 'Either String HybridQuery'.
    mkHybridQuery,
    -- * Setter Functions
    setUserPrompt,
    setLimit, -- Returns Either String HybridQuery
    setAlpha, -- Returns Either String HybridQuery
    -- * Default Values
    defaultAlpha,
    defaultLimit
) where

import qualified Data.Text as T
import Data.Text (Text)
import Data.Either (Either (..)) -- Import constructors for pattern matching if needed
import Control.Monad ((>>=)) -- Make sure bind is available
-- Or: import Data.Function ((&))

-- | Parameters for a Weaviate hybrid search query.
--   Use 'mkHybridQuery' and setter functions to construct values of this type.
data HybridQuery = HybridQuery {
    hqCollection :: !Text   -- ^ __Required.__ The name of the Weaviate collection.
  , hqQuery      :: !Text   -- ^ __Required.__ The user's natural language query prompt.
  , hqLimit      :: !Int    -- ^ The maximum number of results. Validated: must be non-negative.
  , hqAlpha      :: !Double -- ^ Weighting factor ($[0, 1]$). $0$=keyword, $1$=vector. Validated: must be in range $[0, 1]$.
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