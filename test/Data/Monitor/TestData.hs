{-# LANGUAGE OverloadedStrings #-}

module Data.Monitor.TestData
    (
      -- * Test data
      listMetricValuesRequest
      -- * Result
    , queryItem
    , expectedApiVersionItem
    , expectedAuthHeader
    , expectedListMetricValuesPath
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import qualified Data.Monitor.ListMetricValuesRequest as L
import Data.Contract (monitorApiVersion)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Request)
import Network.HTTP.Types (Header, hAuthorization)

-- | Test request for `ListMetricValuesRequest`.
listMetricValuesRequest :: Request
listMetricValuesRequest =
    L.request (testDatastore ! "accessToken") listMetricValuesParams

-- | Test `Params` for `listMetricValuesRequest`.
listMetricValuesParams :: L.Params
listMetricValuesParams = L.Params
    { L._aggregation = testDatastore ! "aggregation"
    , L._metricNames = testDatastore ! "metricnames"
    , L._resourceId  = testDatastore ! "resourceId"
    , L._timespan    = testDatastore ! "timespan"
    }

-- | Derive the query item from `testDatastore` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testDatastore ! n))

-- | The expected API version query item for `getRateCardRequest`.
expectedApiVersionItem :: (ByteString, ByteString)
expectedApiVersionItem = ("api-version", encodeUtf8 monitorApiVersion)

-- | The expected authorization header.
expectedAuthHeader :: Header
expectedAuthHeader =
    (hAuthorization, "Bearer " <> encodeUtf8 (testDatastore ! "accessToken"))

-- | The expected path for `listMetricValuesRequest`. It should:
--
-- #. Starts with the resource ID
--
-- #. Follows by the API Endpoint
expectedListMetricValuesPath :: ByteString
expectedListMetricValuesPath = encodeUtf8 $ T.concat
    [ testDatastore ! "resourceId"
    , "/providers/microsoft.insights/metrics"
    ]

-- | Datastore for test data.
testDatastore :: HashMap Text Text
testDatastore = HM.fromList
    [ ("accessToken", "some-token")
    , ("aggregation", "average,count")
    , ("metricnames", "Percentage CPU,Network In,Network Out")
    , ("resourceId",  "/subscriptions/312-78e8/blah-blah-blah")
    , ("timespan",    "2018-10-08T09:01:10Z/2018-10-08T09:02:10Z")
    ]
