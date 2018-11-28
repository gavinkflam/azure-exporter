{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

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

import Control.Lens (makeLenses, (^.))
import Network.HTTP.Client (Request, parseRequest_, setQueryString)

import Data.AzureRm.Contract (monitorApiVersion)
import Data.AzureRm.Request (addAuthHeader)

-- | Parameters to construct `Request`.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
data Params = Params
    { _aggregation :: {-# UNPACK #-} !Text
    , _metricNames :: {-# UNPACK #-} !Text
    , _resourceId  :: {-# UNPACK #-} !Text
    , _timespan    :: {-# UNPACK #-} !Text
    } deriving Show

makeLenses ''Params

-- | Construct URL from resource URI.
url :: Text -> String
url resourceId' =
    "https://management.azure.com"
    <> unpack resourceId'
    <> "/providers/microsoft.insights/metrics"

-- | Construct query string parameters from `Params`.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
    [ ("api-version", Just $ encodeUtf8 monitorApiVersion)
    , ("aggregation", Just $ encodeUtf8 (p ^. aggregation))
    , ("metricnames", Just $ encodeUtf8 (p ^. metricNames))
    , ("timespan",    Just $ encodeUtf8 (p ^. timespan))
    ]

-- | Construct `Request` from access token and `Params`.
request :: Text -> Params -> Request
request token p =
    setQueryString params $ addAuthHeader token req
  where
    params = queryParams p
    req    = parseRequest_ $ url (p ^. resourceId)
