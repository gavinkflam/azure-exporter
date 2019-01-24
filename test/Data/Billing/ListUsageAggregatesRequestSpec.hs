{-# LANGUAGE OverloadedStrings #-}

-- | Test request construction for the list usage aggregates API.
module Data.Billing.ListUsageAggregatesRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.AzureRm.Contract (billingApiVersion)
import qualified Data.Billing.ListUsageAggregatesRequest as L
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Request, path, queryString, requestHeaders)
import Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import Test.Hspec

-- | Spec for `ListUsageAggregatesRequest`.
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

        it "contains the expected aggregation granularity query item" $
            qItems `shouldContain` [queryItem "aggregationGranularity"]

        it "contains the expected reported start time query item" $
            qItems `shouldContain` [queryItem "reportedStartTime"]

        it "contains the expected reported end time query item" $
            qItems `shouldContain` [queryItem "reportedEndTime"]

        it "contains the expected continuation token query item" $
            qItems `shouldContain` [queryItem "continuationToken"]

        it "contains the expected path" $
            rPath `shouldBe` expectedPath

-- | Test request.
testRequest :: Request
testRequest = L.request (testTexts ! "accessToken") testParams

-- | Test params.
testParams :: L.Params
testParams = L.Params
    { L.subscriptionId         = testTexts ! "subscriptionId"
    , L.aggregationGranularity = testTexts ! "aggregationGranularity"
    , L.reportedStartTime      = testTexts ! "reportedStartTime"
    , L.reportedEndTime        = testTexts ! "reportedEndTime"
    , L.continuationToken      = Just $ testTexts ! "continuationToken"
    }

-- | The expected authorization header.
expectedAuthHeader :: Header
expectedAuthHeader =
    (hAuthorization, "Bearer " <> encodeUtf8 (testTexts ! "accessToken"))

-- | The expected API version query item.
expectedApiVersionItem :: (ByteString, ByteString)
expectedApiVersionItem = ("api-version", encodeUtf8 billingApiVersion)

-- | Derive the query item from `testTexts` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testTexts ! n))

-- | The expected path. It should:
--
-- #. Starts with the subscription ID
--
-- #. Follows by the API Endpoint
expectedPath :: ByteString
expectedPath = encodeUtf8 $ T.concat
    [ "/subscriptions/"
    , testTexts ! "subscriptionId"
    , "/providers/Microsoft.Commerce/UsageAggregates"
    ]

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("accessToken",            "some-token")
    , ("aggregationGranularity", "daily")
    , ("continuationToken",      "something")
    , ("reportedStartTime",      "2018-06-26T08:00:00Z")
    , ("reportedEndTime",        "2018-06-26T08:01:00Z")
    , ("subscriptionId",         "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    ]
