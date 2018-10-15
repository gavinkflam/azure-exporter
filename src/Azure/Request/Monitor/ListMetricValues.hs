{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lists the metric values for a resource.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list>
module Azure.Request.Monitor.ListMetricValues
  (
  -- * Types
    Params (..)
  -- * Request
  , request
  ) where

import Azure.Contract (monitorApiVersion)
import Azure.Data.Monitor.ListMetricValuesResponse (ListMetricValuesResponse)
import Azure.Util.HTTP (addAuthHeader)
import Control.Lens (makeLenses, (^.))
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client (Request, parseRequest_, setQueryString)

-- | Parameters to construct `Request`.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
data Params =
  Params { _aggregation :: Text
         , _metricNames :: Text
         , _resourceId  :: Text
         , _timespan    :: Text
         } deriving Show

makeLenses ''Params

-- | Construct URL from resource URI.
url :: Text -> String
url resourceId =
  "https://management.azure.com"
  <> unpack resourceId
  <> "/providers/microsoft.insights/metrics"

-- | Construct query string parameters from `Params`.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#uri-parameters>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
  [ ("api-version", Just $ toStrict $ encodeUtf8 monitorApiVersion)
  , ("aggregation", Just $ toStrict $ encodeUtf8 (p ^. aggregation))
  , ("metricnames", Just $ toStrict $ encodeUtf8 (p ^. metricNames))
  , ("timespan",    Just $ toStrict $ encodeUtf8 (p ^. timespan))
  ]

-- | Construct `Request` from access token and `Params`.
request :: Text -> Params -> Request
request token p =
  setQueryString params $ addAuthHeader token req
    where params = queryParams p
          req    = parseRequest_ $ url (p ^. resourceId)
