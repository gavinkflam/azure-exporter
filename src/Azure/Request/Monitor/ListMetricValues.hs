{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Request.Monitor.ListMetricValues
  ( Params (..)
  -- Request
  , listMetricValues
  ) where

import Azure.Contract (withAuth)
import Azure.Data.Monitor.ListMetricValuesResponse (ListMetricValuesResponse)
import Control.Lens (makeLenses, (^.))
import Data.Aeson (eitherDecode)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
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
  , ("aggregation", Just $ toStrict $ encodeUtf8 $ p ^. aggregation)
  , ("metricnames", Just $ toStrict $ encodeUtf8 $ p ^. metricNames)
  , ("timespan",    Just $ toStrict $ encodeUtf8 $ p ^. timespan)
  ]

-- Request
listMetricValues :: Text -> Params -> IO (Either String ListMetricValuesResponse)
listMetricValues token p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ listMetricValuesUrl $ p ^. resourceId
  let req' = setQueryString (queryParams p) $ withAuth token req
  res <- httpLbs req' manager
  return $ eitherDecode $ responseBody res
