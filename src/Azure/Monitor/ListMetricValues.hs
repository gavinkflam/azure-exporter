{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Monitor.ListMetricValues
  ( Params (..)
  -- Request
  , listMetricValues
  ) where

import Azure.Monitor.Data.ListMetricValuesResponse (ListMetricValuesResponse)
import Azure.Monitor.Contract (withAuth)
import Control.Lens (makeLenses, (^.))
import Data.Aeson (decode)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Request parameters
data Params =
  Params { _aggregation :: Text
         , _metricNames :: Text
         , _resourceId  :: Text
         , _timespan    :: Text
         } deriving Show

makeLenses ''Params

-- Request utilities
listMetricValuesUrl :: Text -> String
listMetricValuesUrl resourceId =
  "https://management.azure.com"
  <> unpack resourceId
  <> "/providers/microsoft.insights/metrics"

queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
  [ ("api-version", Just "2018-01-01")
  , ("aggregation", Just $ encodeUtf8 $ p ^. aggregation)
  , ("metricnames", Just $ encodeUtf8 $ p ^. metricNames)
  , ("timespan",    Just $ encodeUtf8 $ p ^. timespan)
  ]

-- Request
listMetricValues :: Text -> Params -> IO (Maybe ListMetricValuesResponse)
listMetricValues token p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ listMetricValuesUrl $ p ^. resourceId
  let req' = setQueryString (queryParams p) $ withAuth token req
  res <- httpLbs req' manager
  return $ decode $ responseBody res
