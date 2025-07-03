{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}


-- |
-- Module      : Weaviate.Client
-- Description : Client functions for interacting with a Weaviate instance over HTTP.
-- Copyright   : (c) 2025 Wayne "h-alice" Hong
-- License     : AGPL-3.0
-- Maintainer  : admin@halice.art
-- Stability   : experimental
-- Portability : POSIX
--
-- This module provides high-level functions to execute specific Weaviate queries,
-- currently focusing on the hybrid search defined in 'Weaviate.Query' and
-- 'Weaviate.GraphQL'. It handles HTTP communication, basic error handling,
-- and response parsing. It integrates with the 'Either'-based validation
-- from 'Weaviate.Query'.
--
-- == Core Functions:
--
--   * 'weaviateHybridSearch': Takes potentially validated query parameters ('Either String HybridQuery').
--     If parameters are valid ('Right'), sends the query and returns the raw JSON
--     response ('Value') or an HTTP/network error. If parameters are invalid ('Left'),
--     propagates the parameter error.
--   * 'getTopNDocuments': Parses the result of 'weaviateHybridSearch' ('Either String Value')
--     into structured Haskell types ('WeaviateResponse') and extracts the content
--     of the top N documents. Handles input errors and parsing errors.
--
-- == Limitations:
--
--   * This client is currently hardcoded to perform the specific hybrid search.
--
-- == Example Usage (with Either-based Query):
--
-- > import Weaviate.Query (mkHybridQuery, setUserPrompt, setLimit, setAlpha)
-- > import qualified Weaviate.Client as WC
-- > import qualified Data.Text as T
-- > import qualified Data.Text.IO as TIO
-- > import Data.Either (Either(..))
-- >
-- > main :: IO ()
-- > main = do
-- >     -- Start building a query for the "Document" collection with chain validation
-- >     -- Chain validating setters using >>=
-- >     let eitherParams :: Either String HybridQuery
-- >         eitherParams = mkHybridQuery (T.pack "Document")                -- Start the Either chain
-- >                        >>= setUserPrompt (T.pack "Benefits of Haskell") -- Assume setUserPrompt doesn't fail
-- >                        >>= setLimit 10      -- Apply setLimit
-- >                        >>= setAlpha 0.6     -- Apply setAlpha
-- >                        -- >>= setLimit (-5) -- This would cause a Left with an error message
-- >
-- >     let weaviateEndpoint = "http://localhost:8080/v1/graphql" -- Adjust URL
-- >
-- >     -- 2. Call the search function (handles Left case internally)
-- >     putStrLn $ "Sending query to " ++ weaviateEndpoint ++ "..."
-- >     eRawResponse <- WC.weaviateHybridSearch weaviateEndpoint eitherParams
-- >
-- >     -- 3. Process the response
-- >     putStrLn "\n--- Top N Documents ---"
-- >     let numDocs = 3
-- >     case WC.getTopNDocuments eRawResponse numDocs of
-- >         Left errMsg -> putStrLn $ "Error: " ++ errMsg
-- >         Right docs  -> do
-- >             putStrLn $ "Found " ++ show (length docs) ++ " documents (max " ++ show numDocs ++ "):"
-- >             mapM_ (\doc -> TIO.putStrLn "---" >> TIO.putStrLn doc) docs
-- >             putStrLn "End of documents."
module Weaviate.Client (
    -- * Primary Client Functions
    weaviateHybridSearch,
    getDocuments
) where

-- Internal Library Imports
import Weaviate.Query (HybridQuery(..))
import Weaviate.GraphQL (mkGraphQLQueryCL, GraphQLRequest(..))
import Weaviate.Response (WeaviateResponse(..), GetResponseData(..), GetDocument(..), QueryResult(..), OriginalDocument(..))

-- External Library Imports
import qualified Data.Aeson           as A (encode, object, Value, Result(..), fromJSON, eitherDecode)
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL (ByteString)
import qualified Network.Wreq         as W (postWith, defaults, header, responseStatus, statusCode, statusMessage, responseBody, Response)
import qualified Data.Text            as T (Text, unpack)
import qualified Data.Text.Encoding   as TE (decodeUtf8)
import Control.Lens                 ((&), (.~), (^.))
import Control.Exception            (try, SomeException)
import Text.Printf                  (printf)

-- | Sends the specific hybrid search query to a Weaviate GraphQL endpoint.
--   Takes an 'Either String HybridQuery' as input. If 'Left', it returns the
--   query error immediately. If 'Right', it proceeds with the HTTP request.
--
--   Handles basic network and HTTP errors during the request.
--
--   Returns:
--     * `Left String`: An error message detailing the failure (invalid query params, HTTP error, non-2xx status, JSON decoding error).
--     * `Right Value`: The raw JSON value parsed from the response body on success.
weaviateHybridSearch :: String                  -- ^ Weaviate GraphQL endpoint URL.
                        -> Either String HybridQuery -- ^ Validated hybrid query parameters (or error).
                        -> IO (Either String A.Value)  -- ^ Result (error or raw JSON Value) in IO.

-- Case 1: Query construction already failed
weaviateHybridSearch _ (Left queryErr) =
    return $ Left ("Invalid query parameters provided: " ++ queryErr) -- Use return as type is IO(...)

-- Case 2: Query construction succeeded, proceed with HTTP request
weaviateHybridSearch weaviateUrl (Right hq) = do
    -- Construct the GraphQL query string and request body
    let graphqlQueryString = mkGraphQLQueryCL (hqCollection hq) (hqLimit hq)
    let requestPayload = GraphQLRequest {
        gqlQuery = graphqlQueryString,
        gqlVariables = Just $ A.object [ "q" .= hqQuery hq, "a" .= hqAlpha hq ],
        gqlOperationName = Just "HybridSearch"
    }
    let opts = W.defaults & W.header "Content-Type" .~ ["application/json"]

    -- Perform the HTTP POST request, catching exceptions
    eResult <- try (W.postWith opts weaviateUrl (A.encode requestPayload))
                :: IO (Either SomeException (W.Response BL.ByteString))

    -- Process the result
    case eResult of
        Left networkErr -> return $ Left (printf "HTTP request failed: %s" (show networkErr))
        Right resp -> do
            let statusCode = resp ^. W.responseStatus . W.statusCode
            let statusMsgBytes = resp ^. W.responseStatus . W.statusMessage
            let responseBody = resp ^. W.responseBody

            if statusCode >= 200 && statusCode < 300
                then -- Success: Try to parse body as JSON Value
                    case A.eitherDecode responseBody of -- Use eitherDecode here for Value
                        Left jsonErr -> return $ Left (printf "JSON decoding of successful response failed: %s Raw body: %s" jsonErr (show responseBody))
                        Right jsonValue -> return $ Right jsonValue
                else -- Weaviate returned an error status
                    let statusMsgText = TE.decodeUtf8 statusMsgBytes
                    in return $ Left (printf "Weaviate request failed with status %d: %s Response body: %s"
                                    statusCode (T.unpack statusMsgText) (show responseBody))

origDocument :: QueryResult -> T.Text
origDocument qr =
    case qrOrig qr of
        [] -> ""
        (doc:_) -> odContent doc

getDocuments :: Either String A.Value
                    -> Either String [T.Text]

-- Case 1: Input was already an error from weaviateHybridSearch pipeline      
getDocuments (Left prevErr) =
    Left ("Cannot get documents because previous step failed: " ++ prevErr) -- No 'return'

-- Case 2: Valid JSON input from previous step
getDocuments (Right jsonValue) =
    -- Attempt to parse the JSON Value directly using fromJSON
    case A.fromJSON jsonValue :: A.Result WeaviateResponse of
            A.Error parseErr -> Left $ "Failed to parse JSON Value into WeaviateResponse: " ++ parseErr ++ 
                "\nRaw JSON: " ++ show jsonValue
            A.Success wr ->
                -- Extract results list
                let results = gdResults . grdGet . wrData $ wr
                    -- Check if empty and return results
                    in if null results
                        then Left "No similar documents found in the response."
                        else Right $ filter (/= "") $ map origDocument results
