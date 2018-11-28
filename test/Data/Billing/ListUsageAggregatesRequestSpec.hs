{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the list usage aggregates API.
module Data.Billing.ListUsageAggregatesRequestSpec
    (
      -- * Spec
      spec
    ) where

import Network.HTTP.Client (path, queryString, requestHeaders)
import Network.HTTP.Types (parseSimpleQuery)
import Test.Hspec

import qualified Data.Billing.TestData as D

-- | Spec for `ListUsageAggregates`.
spec :: Spec
spec = do
    let headers = requestHeaders D.listUsageAggregatesRequest
        rPath   = path D.listUsageAggregatesRequest
        qItems  = parseSimpleQuery $ queryString D.listUsageAggregatesRequest

    describe "request" $ do
        it "contains the expected authorization header" $
            headers `shouldContain` [D.expectedAuthHeader]

        it "contains the expected api-version query item" $
            qItems `shouldContain` [D.expectedApiVersionItem]

        it "contains the expected aggregation granularity query item" $
            qItems `shouldContain` [D.queryItem "aggregationGranularity"]

        it "contains the expected reported start time query item" $
            qItems `shouldContain` [D.queryItem "reportedStartTime"]

        it "contains the expected reported end time query item" $
            qItems `shouldContain` [D.queryItem "reportedEndTime"]

        it "contains the expected continuation token query item" $
            qItems `shouldContain` [D.queryItem "continuationToken"]

        it "contains the expected path" $
            rPath `shouldBe` D.expectedListUsageAggregatesPath
