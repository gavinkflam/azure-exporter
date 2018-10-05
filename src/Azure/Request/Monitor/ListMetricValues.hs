{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Request.Monitor.ListMetricValues
  ( Params (..)
  -- Request
  , request
  ) where

import Azure.Contract (withAuth)
import Azure.Data.Monitor.ListMetricValuesResponse (ListMetricValuesResponse)
import Control.Lens (makeLenses, (^.))
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client (Request, parseRequest_, setQueryString)

-- Request parameters
data Params =
  Params { _aggregation :: Text
         , _metricNames :: Text
         , _resourceId  :: Text
         , _timespan    :: Text
         } deriving Show

makeLenses ''Params

-- Request utilities
url :: Text -> String
url resourceId =
  "https://management.azure.com"
  <> unpack resourceId
  <> "/providers/microsoft.insights/metrics"

queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
  [ ("api-version", Just "2018-01-01")
  , ("aggregation", Just $ toStrict $ encodeUtf8 $ p ^. aggregation)
  , ("metricnames", Just $ toStrict $ encodeUtf8 $ p ^. metricNames)
  , ("timespan",    Just $ toStrict $ encodeUtf8 $ p ^. timespan)
  ]

-- Request
request :: Text -> Params -> Request
request token p =
  setQueryString params $ withAuth token $ parseRequest_ $ url $ p ^. resourceId
    where params = queryParams p
