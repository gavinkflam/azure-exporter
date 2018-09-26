{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Monitor.ListMetricValues
  ( ListMetricValuesParams (..)
  , ListMetricValuesResponse (..)
  , listMetricValues
  ) where

import AzureExporter.Monitor.Data.ListMetricValuesResponse (ListMetricValuesResponse)
import AzureExporter.Monitor.Contract (withAuth)
import Control.Lens (makeLenses, (^.))
import Data.Aeson (decode)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Request parameters
data ListMetricValuesParams =
  ListMetricValuesParams { _aggregation :: Text
                         , _metricNames :: Text
                         , _resourceId  :: Text
                         , _timespan    :: Text
                         } deriving Show

makeLenses ''ListMetricValuesParams

-- Request utilities
listMetricValuesUrl :: Text -> String
listMetricValuesUrl resourceId =
  "https://management.azure.com"
  <> unpack resourceId
  <> "/providers/microsoft.insights/metrics"

queryParams :: ListMetricValuesParams -> [(ByteString, Maybe ByteString)]
queryParams p =
  [ ("api-version", Just "2018-01-01")
  , ("aggregation", Just $ encodeUtf8 $ p ^. aggregation)
  , ("metricnames", Just $ encodeUtf8 $ p ^. metricNames)
  , ("timespan",    Just $ encodeUtf8 $ p ^. timespan)
  ]

listMetricValues :: Text -> ListMetricValuesParams -> IO (Maybe ListMetricValuesResponse)
listMetricValues token p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ listMetricValuesUrl $ p ^. resourceId
  let req' = setQueryString (queryParams p) $ withAuth token req
  res <- httpLbs req' manager
  return $ decode $ responseBody res
