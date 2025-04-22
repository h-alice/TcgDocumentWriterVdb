module Weaviate.Query (
    HybridQuery(..),
    hyQueryCollection,
    setLimit,
    setUserPrompt,
    setAlpha
) where

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

import qualified Data.Text   as T
import Data.Text             (Text)

-- | Hybrid search parameters
data HybridQuery = HybridQuery {
    hqCollection :: Text, -- The collection name (e.g., "Document")
    hqQuery :: Text,      -- The user's search query text
    hqLimit :: Int,       -- Limit the number of results
    hqAlpha :: Double     -- Balance between vector/keyword search (0=keyword, 1=vector)
} deriving (Show, Eq)     -- Deriving Show for easy printing/debugging
--

-- | Define default values
defaultAlpha :: Double
defaultAlpha = 0.5

defaultLimit :: Int
defaultLimit = 3

-- | Initial builder function. Requires the collection name.
hyQueryCollection :: Text -> HybridQuery
hyQueryCollection collection = HybridQuery {
    hqCollection = collection,
    hqQuery = T.empty,
    hqLimit = defaultLimit,
    hqAlpha = defaultAlpha
}

-- | Set the alpha value for the hybrid query.
setAlpha :: Double -> HybridQuery -> HybridQuery
setAlpha newAlpha hq = hq { hqAlpha = newAlpha }

-- | Set the limit for the hybrid query.
setLimit :: Int -> HybridQuery -> HybridQuery
setLimit newLimit hq = hq { hqLimit = newLimit }

-- | Set the user prompt.
setUserPrompt :: Text -> HybridQuery -> HybridQuery
setUserPrompt newQuery hq = hq { hqQuery = newQuery }
