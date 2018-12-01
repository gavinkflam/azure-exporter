{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the get rate card API.
module Data.Billing.GetRateCardRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.AzureRm.Contract (billingApiVersion)
import qualified Data.Billing.GetRateCardRequest as R
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Request, path, queryString, requestHeaders)
import Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import Test.Hspec

-- | Spec for `GetRateCard`.
spec :: Spec
spec = do
    let headers = requestHeaders testRequest
        rPath   = path testRequest
        qItems  = parseSimpleQuery $ queryString testRequest

    describe "request" $ do
        it "contains the expected authorization header" $
            headers `shouldContain` [expectedAuthHeader]

        it "contains the expected api-version query item" $
            qItems `shouldContain` [expectedApiVersionItem]

        it "contains the expected $filter query item" $
            qItems `shouldContain` [("$filter", expectedFilterQuery)]

        it "contains the expected path" $
            rPath `shouldBe` expectedPath

-- | Test request.
testRequest :: Request
testRequest = R.request (testTexts ! "accessToken") testParams

-- | Test params.
testParams :: R.Params
testParams = R.Params
    { R._subscriptionId = testTexts ! "subscriptionId"
    , R._offerId        = testTexts ! "offerId"
    , R._currency       = testTexts ! "currency"
    , R._locale         = testTexts ! "locale"
    , R._regionInfo     = testTexts ! "regionInfo"
    }

-- | The expected authorization header.
expectedAuthHeader :: Header
expectedAuthHeader =
    (hAuthorization, "Bearer " <> encodeUtf8 (testTexts ! "accessToken"))

-- | The expected API version query item.
expectedApiVersionItem :: (ByteString, ByteString)
expectedApiVersionItem = ("api-version", encodeUtf8 billingApiVersion)

-- | The expected filter query for `getRateCardRequest`.
expectedFilterQuery :: ByteString
expectedFilterQuery = encodeUtf8 $ T.concat
    [ "OfferDurableId eq '"
    , testTexts ! "offerId"
    , "' and Currency eq '"
    , testTexts ! "currency"
    , "' and Locale eq '"
    , testTexts ! "locale"
    , "' and RegionInfo eq '"
    , testTexts ! "regionInfo"
    , "'"
    ]

-- | The expected path for `getRateCardRequest`. It should:
--
-- #. Starts with the subscription ID
--
-- #. Follows by the API Endpoint
expectedPath :: ByteString
expectedPath = encodeUtf8 $ T.concat
    [ "/subscriptions/"
    , testTexts ! "subscriptionId"
    , "/providers/Microsoft.Commerce/RateCard"
    ]

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("accessToken",            "some-token")
    , ("currency",               "USD")
    , ("locale",                 "en-US")
    , ("offerId",                "MS-AZR-0003p")
    , ("regionInfo",             "US")
    , ("subscriptionId",         "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    ]
