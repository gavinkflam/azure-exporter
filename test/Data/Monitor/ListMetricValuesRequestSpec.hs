{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the list metric values API.
module Data.Monitor.ListMetricValuesRequestSpec
    (
      -- * Spec
      spec
    ) where

import Network.HTTP.Client (path, queryString, requestHeaders)
import Network.HTTP.Types (parseSimpleQuery)
import Test.Hspec

import qualified Data.Monitor.TestData as D

-- | Spec for `ListMetricValues`.
spec :: Spec
spec = do
    let headers = requestHeaders D.listMetricValuesRequest
        rPath   = path D.listMetricValuesRequest
        qItems  = parseSimpleQuery $ queryString D.listMetricValuesRequest

    describe "request" $ do
        it "contains authorization header" $
            headers `shouldContain` [D.expectedAuthHeader]

        it "contains api-version query item" $
            qItems `shouldContain` [D.expectedApiVersionItem]

        it "contains aggregation query item" $
            qItems `shouldContain` [D.queryItem "aggregation"]

        it "contains metricnames query item" $
            qItems `shouldContain` [D.queryItem "metricnames"]

        it "contains timespan query item" $
            qItems `shouldContain` [D.queryItem "timespan"]

        it "contains the expected path" $
            rPath `shouldBe` D.expectedListMetricValuesPath
