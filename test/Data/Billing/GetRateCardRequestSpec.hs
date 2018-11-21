{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the get rate card API.
module Data.Billing.GetRateCardRequestSpec
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

import Data.Billing.GetRateCardRequest
import Data.Contract (billingApiVersion)
import qualified Data.Dummy.Text as T
import Util.Text (toBS)

-- | Spec for `GetRateCard`.
spec :: Spec
spec = do
    let req    = request T.accessToken params
        qItems = parseSimpleQuery $ queryString req

    describe "request" $ do
        it "contains authorization header" $
            requestHeaders req `shouldContain` [authHeader]

        it "contains api-version query item" $
            qItems `shouldContain` [("api-version", toBS billingApiVersion)]

        it "contains $filter query item" $
            qItems `shouldContain` [("$filter", toBS T.filterQuery)]

        it "contains the expected path" $
            C.unpack (path req) `shouldBe` expectedPath

-- | Dummy `Params` item.
params :: Params
params = Params
    { _subscriptionId = T.subscriptionId
    , _offerId        = T.offerId
    , _currency       = T.currency
    , _locale         = T.locale
    , _regionInfo     = T.regionInfo
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
    <> "/providers/Microsoft.Commerce/RateCard"

-- | Dummy authorization header.
authHeader :: Header
authHeader =
    (hAuthorization, toStrict $ encodeUtf8 ("Bearer " <> T.accessToken))
