{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the list metric values API.
module Data.Monitor.ListMetricValuesRequestSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Char8 as C
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Client (path, queryString, requestHeaders)
import Network.HTTP.Types (Header, hAuthorization, parseSimpleQuery)
import Test.Hspec

import Data.Contract (monitorApiVersion)
import qualified Data.Dummy.Text as T
import Data.Monitor.ListMetricValuesRequest
import Util.Text (toBS)

-- | Spec for `ListMetricValues`.
spec :: Spec
spec = do
    let req    = request T.accessToken params
        qItems = parseSimpleQuery $ queryString req

    describe "request" $ do
        it "contains authorization header" $
            requestHeaders req `shouldContain` [authHeader]

        it "contains api-version query item" $
            qItems `shouldContain` [("api-version", toBS monitorApiVersion)]

        it "contains aggregation query item" $
            qItems `shouldContain` [("aggregation", toBS T.aggregation)]

        it "contains metricnames query item" $
            qItems `shouldContain` [("metricnames", toBS T.metricNames)]

        it "contains timespan query item" $
            qItems `shouldContain` [("timespan", toBS T.timespan)]

        it "contains the expected path" $
            C.unpack (path req) `shouldBe` expectedPath

-- | Dummy `Params` item.
params :: Params
params = Params
    { _aggregation = T.aggregation
    , _metricNames = T.metricNames
    , _resourceId  = T.resourceId
    , _timespan    = T.timespan
    }

-- | The expected path should
--
-- #. Starts with the resource ID
--
-- #. Follows by the API Endpoint
expectedPath :: String
expectedPath = unpack $ T.resourceId <> "/providers/microsoft.insights/metrics"

-- | Dummy authorization header.
authHeader :: Header
authHeader =
    (hAuthorization, toStrict $ encodeUtf8 ("Bearer " <> T.accessToken))
