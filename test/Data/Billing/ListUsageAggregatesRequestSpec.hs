{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the list usage aggregates API.
module Data.Billing.ListUsageAggregatesRequestSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Client (requestHeaders, path, queryString)
import Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import Test.Hspec

import Data.Billing.ListUsageAggregatesRequest
import Data.Contract (billingApiVersion)
import qualified Data.Dummy.Text as T
import qualified Data.Dummy.Time as M
import Util.Text (toBS)

-- | Spec for `ListUsageAggregates`.
spec :: Spec
spec = do
    let req    = request T.accessToken params
        qItems = parseSimpleQuery $ queryString req

    describe "request" $ do
        it "contains authorization header" $
            requestHeaders req `shouldContain` [authHeader]

        it "contains api-version query item" $
            qItems `shouldContain` [("api-version", toBS billingApiVersion)]

        it "contains aggregation granularity query item" $
            qItems `shouldContain` [("aggregationGranularity", "daily")]

        it "contains reported start time query item" $
            qItems `shouldContain` [("reportedStartTime", toBS M.timestampFrom)]

        it "contains reported end time query item" $
            qItems `shouldContain` [("reportedEndTime", toBS M.timestampTo)]

        it "contains continuation token query item" $
            qItems `shouldContain` [("continuationToken", "something")]

        it "contains the expected path" $
            C.unpack (path req) `shouldBe` expectedPath

-- | Dummy `Params` item.
params :: Params
params = Params
    { _subscriptionId         = T.subscriptionId
    , _aggregationGranularity = "daily"
    , _reportedStartTime      = M.timestampFrom
    , _reportedEndTime        = M.timestampTo
    , _continuationToken      = Just "something"
    }

-- | The expected path should
--
-- #. Starts with the subscription ID
--
-- #. Follows by the API Endpoint
expectedPath :: String
expectedPath =
    "/subscriptions/"
    <> unpack T.subscriptionId
    <> "/providers/Microsoft.Commerce/UsageAggregates"

-- | Dummy authorization header.
authHeader :: Header
authHeader =
    (hAuthorization, toStrict $ encodeUtf8 ("Bearer " <> T.accessToken))
