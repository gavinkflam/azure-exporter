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
import           Data.ByteString.Lazy (toStrict)
import           Data.Text.Lazy (Text)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified DummyText as T
import           Expectations
import           Network.HTTP.Client (requestHeaders, queryString)
import           Network.HTTP.Types (Header, hAuthorization, urlEncode)
import           Test.Hspec

-- | Spec for `ListMetricValues`.
spec :: Spec
spec = do
  let req = request T.accessToken params

  describe "request" $ do
    it "contains authorization header" $
      requestHeaders req `shouldContain` [authHeader]

    it "contains the expected query string" $
      queryString req `shouldBe` expectedQueryString

-- | Dummy `Params` item.
params =
  Params { _aggregation = T.aggregation
         , _metricNames = T.metricNames
         , _resourceId  = T.resourceId
         , _timespan    = T.timespan
         }

-- | Expected query string constructed manually.
expectedQueryString :: BS.ByteString
expectedQueryString =
  "?api-version=2018-01-01&" <>
    "aggregation=" <> urlEncodeText T.aggregation <> "&" <>
    "metricnames=" <> urlEncodeText T.metricNames <> "&" <>
    "timespan=" <> urlEncodeText T.timespan

-- | URL encode a `Text`.
urlEncodeText :: Text -> BS.ByteString
urlEncodeText t = urlEncode True $ toStrict $ encodeUtf8 t

-- | Dummy authorization header.
authHeader :: Header
authHeader =
  (hAuthorization, toStrict $ encodeUtf8 ("Bearer " <> T.accessToken))
