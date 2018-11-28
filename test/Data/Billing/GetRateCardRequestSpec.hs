{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the get rate card API.
module Data.Billing.GetRateCardRequestSpec
    (
      -- * Spec
      spec
    ) where

import Network.HTTP.Client (path, queryString, requestHeaders)
import Network.HTTP.Types (parseSimpleQuery)
import Test.Hspec

import qualified Data.Billing.TestData as D

-- | Spec for `GetRateCard`.
spec :: Spec
spec = do
    let headers = requestHeaders D.getRateCardRequest
        rPath   = path D.getRateCardRequest
        qItems  = parseSimpleQuery $ queryString D.getRateCardRequest

    describe "request" $ do
        it "contains the expected authorization header" $
            headers `shouldContain` [D.expectedAuthHeader]

        it "contains the expected api-version query item" $
            qItems `shouldContain` [D.expectedApiVersionItem]

        it "contains the expected $filter query item" $
            qItems `shouldContain` [("$filter", D.expectedFilterQuery)]

        it "contains the expected path" $
            rPath `shouldBe` D.expectedGetRateCardPath
