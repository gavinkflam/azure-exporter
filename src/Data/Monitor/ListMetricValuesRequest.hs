{-# LANGUAGE OverloadedStrings #-}

-- | Lists the metric values for a resource.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list>
module Data.Monitor.ListMetricValuesRequest
    (
      -- * Types
      Params (..)
      -- * Request
    , request
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client (Request, parseRequest_, setQueryString)

import Data.AzureRm.Contract (monitorApiVersion)
import Data.AzureRm.Request (addAuthHeader)

-- | Parameters to construct request.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
data Params = Params
    { aggregation :: {-# UNPACK #-} !Text
    , metricNames :: {-# UNPACK #-} !Text
    , resourceId  :: {-# UNPACK #-} !Text
    , timespan    :: {-# UNPACK #-} !Text
    } deriving Show

-- | Construct URL from resource URI.
url :: Text -> String
url resourceId' =
    "https://management.azure.com"
    <> unpack resourceId'
    <> "/providers/microsoft.insights/metrics"

-- | Construct query string parameters from params.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
    [ ("api-version", Just $ encodeUtf8 monitorApiVersion)
    , ("aggregation", Just $ encodeUtf8 $ aggregation p)
    , ("metricnames", Just $ encodeUtf8 $ metricNames p)
    , ("timespan",    Just $ encodeUtf8 $ timespan p)
    ]

-- | Construct request from access token and params.
request :: Text -> Params -> Request
request token p =
    setQueryString params $ addAuthHeader token req
  where
    params = queryParams p
    req    = parseRequest_ $ url $ resourceId p
