{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test `Request` construction for the list metric values API.
module Azure.Request.Monitor.ListMetricValuesSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Request.Monitor.ListMetricValues
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import           Data.ByteString.Lazy (toStrict)
import           Data.Text.Lazy (Text, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified DummyText as T
import           Expectations
import           Network.HTTP.Client (requestHeaders, path, queryString)
import           Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import           Test.Hspec

-- | Spec for `ListMetricValues`.
spec :: Spec
spec = do
  let req    = request T.accessToken params
      qItems = parseSimpleQuery $ queryString req

  describe "request" $ do
    it "contains authorization header" $
      requestHeaders req `shouldContain` [authHeader]

    it "contains api-version query item" $
      qItems `shouldContain` [("api-version", "2018-01-01")]

    it "contains aggregation query item" $
      qItems `shouldContain` [("aggregation", toBS T.aggregation)]

    it "contains metricnames query item" $
      qItems `shouldContain` [("metricnames", toBS T.metricNames)]

    it "contains timespan query item" $
      qItems `shouldContain` [("timespan", toBS T.timespan)]

    it "contains the expected path" $
      C.unpack (path req) `shouldBe` expectedPath

-- | Dummy `Params` item.
params =
  Params { _aggregation = T.aggregation
         , _metricNames = T.metricNames
         , _resourceId  = T.resourceId
         , _timespan    = T.timespan
         }

-- |
-- The expected path should
--
-- 1. Starts with the resource ID
-- 2. Follows by the API Endpoint
expectedPath :: String
expectedPath = unpack $ T.resourceId <> "/providers/microsoft.insights/metrics"

-- | Encode a Lazy `Text` to a strict `ByteString`
toBS :: Text -> BS.ByteString
toBS = toStrict . encodeUtf8

-- | Dummy authorization header.
authHeader :: Header
authHeader =
  (hAuthorization, toStrict $ encodeUtf8 ("Bearer " <> T.accessToken))
