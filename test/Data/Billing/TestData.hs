{-# LANGUAGE OverloadedStrings #-}

module Data.Billing.TestData
    (
      -- * Test data
      getRateCardRequest
    , listUsageAggregatesRequest
      -- * Result
    , queryItem
    , expectedApiVersionItem
    , expectedAuthHeader
    , expectedFilterQuery
    , expectedGetRateCardPath
    , expectedListUsageAggregatesPath
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.AzureRm.Contract (billingApiVersion)
import qualified Data.Billing.GetRateCardRequest as R
import qualified Data.Billing.ListUsageAggregatesRequest as L
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Request)
import Network.HTTP.Types (Header, hAuthorization)

-- | Test request for `GetRateCardRequest`.
getRateCardRequest :: Request
getRateCardRequest =
    R.request (testDatastore ! "accessToken") getRateCardParams

-- | Test request for `ListUsageAggregatesRequest`.
listUsageAggregatesRequest :: Request
listUsageAggregatesRequest =
    L.request (testDatastore ! "accessToken") listUsageAggregatesParams

-- | Test `Params` for `getRateCardRequest`.
getRateCardParams :: R.Params
getRateCardParams = R.Params
    { R._subscriptionId = testDatastore ! "subscriptionId"
    , R._offerId        = testDatastore ! "offerId"
    , R._currency       = testDatastore ! "currency"
    , R._locale         = testDatastore ! "locale"
    , R._regionInfo     = testDatastore ! "regionInfo"
    }

-- | Test `Params` for `listUsageAggregatesRequest`.
listUsageAggregatesParams :: L.Params
listUsageAggregatesParams = L.Params
    { L._subscriptionId         = testDatastore ! "subscriptionId"
    , L._aggregationGranularity = testDatastore ! "aggregationGranularity"
    , L._reportedStartTime      = testDatastore ! "reportedStartTime"
    , L._reportedEndTime        = testDatastore ! "reportedEndTime"
    , L._continuationToken      = Just $ testDatastore ! "continuationToken"
    }

-- | Derive the query item from `testDatastore` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testDatastore ! n))

-- | The expected API version query item for `getRateCardRequest`.
expectedApiVersionItem :: (ByteString, ByteString)
expectedApiVersionItem = ("api-version", encodeUtf8 billingApiVersion)

-- | The expected authorization header for `getRateCardRequest`.
expectedAuthHeader :: Header
expectedAuthHeader =
    (hAuthorization, "Bearer " <> encodeUtf8 (testDatastore ! "accessToken"))

-- | The expected filter query for `getRateCardRequest`.
expectedFilterQuery :: ByteString
expectedFilterQuery = encodeUtf8 $ T.concat
    [ "OfferDurableId eq '"
    , testDatastore ! "offerId"
    , "' and Currency eq '"
    , testDatastore ! "currency"
    , "' and Locale eq '"
    , testDatastore ! "locale"
    , "' and RegionInfo eq '"
    , testDatastore ! "regionInfo"
    , "'"
    ]

-- | The expected path for `getRateCardRequest`. It should:
--
-- #. Starts with the subscription ID
--
-- #. Follows by the API Endpoint
expectedGetRateCardPath :: ByteString
expectedGetRateCardPath = encodeUtf8 $ T.concat
    [ "/subscriptions/"
    , testDatastore ! "subscriptionId"
    , "/providers/Microsoft.Commerce/RateCard"
    ]

-- | The expected path for `listUsageAggregatesRequest`. It should:
--
-- #. Starts with the subscription ID
--
-- #. Follows by the API Endpoint
expectedListUsageAggregatesPath :: ByteString
expectedListUsageAggregatesPath = encodeUtf8 $ T.concat
    [ "/subscriptions/"
    , testDatastore ! "subscriptionId"
    , "/providers/Microsoft.Commerce/UsageAggregates"
    ]

-- | Datastore for test data.
testDatastore :: HashMap Text Text
testDatastore = HM.fromList
    [ ("accessToken",            "some-token")
    , ("aggregationGranularity", "daily")
    , ("continuationToken",      "something")
    , ("currency",               "USD")
    , ("locale",                 "en-US")
    , ("offerId",                "MS-AZR-0003p")
    , ("regionInfo",             "US")
    , ("reportedStartTime",      "2018-06-26T08:00:00Z")
    , ("reportedEndTime",        "2018-06-26T08:01:00Z")
    , ("subscriptionId",         "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    ]
