{-# LANGUAGE OverloadedStrings #-}

-- | Test request construction for the list metric values API.
module Data.Monitor.ListMetricValuesRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.AzureRm.Contract (monitorApiVersion)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.Monitor.ListMetricValuesRequest as L
import Network.HTTP.Client (Request, path, queryString, requestHeaders)
import Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import Test.Hspec

-- | Spec for `ListMetricValuesRequest`.
spec :: Spec
spec = do
    let headers = requestHeaders testRequest
        rPath   = path testRequest
        qItems  = parseSimpleQuery $ queryString testRequest

    describe "request" $ do
        it "contains authorization header" $
            headers `shouldContain` [expectedAuthHeader]

        it "contains api-version query item" $
            qItems `shouldContain` [expectedApiVersionItem]

        it "contains aggregation query item" $
            qItems `shouldContain` [queryItem "aggregation"]

        it "contains metricnames query item" $
            qItems `shouldContain` [queryItem "metricnames"]

        it "contains timespan query item" $
            qItems `shouldContain` [queryItem "timespan"]

        it "contains the expected path" $
            rPath `shouldBe` expectedPath

-- | Test request.
testRequest :: Request
testRequest =
    L.request (testTexts ! "accessToken") testParams

-- | Test params.
testParams :: L.Params
testParams = L.Params
    { L._aggregation = testTexts ! "aggregation"
    , L._metricNames = testTexts ! "metricnames"
    , L._resourceId  = testTexts ! "resourceId"
    , L._timespan    = testTexts ! "timespan"
    }

-- | Derive the query item from `testTexts` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testTexts ! n))

-- | The expected API version query item for `getRateCardRequest`.
expectedApiVersionItem :: (ByteString, ByteString)
expectedApiVersionItem = ("api-version", encodeUtf8 monitorApiVersion)

-- | The expected authorization header.
expectedAuthHeader :: Header
expectedAuthHeader =
    (hAuthorization, "Bearer " <> encodeUtf8 (testTexts ! "accessToken"))

-- | The expected path for `listMetricValuesRequest`. It should:
--
-- #. Starts with the resource ID
--
-- #. Follows by the API Endpoint
expectedPath :: ByteString
expectedPath = encodeUtf8 $ T.concat
    [ testTexts ! "resourceId"
    , "/providers/microsoft.insights/metrics"
    ]

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("accessToken", "some-token")
    , ("aggregation", "average,count")
    , ("metricnames", "Percentage CPU,Network In,Network Out")
    , ("resourceId",  "/subscriptions/312-78e8/blah-blah-blah")
    , ("timespan",    "2018-10-08T09:01:10Z/2018-10-08T09:02:10Z")
    ]
