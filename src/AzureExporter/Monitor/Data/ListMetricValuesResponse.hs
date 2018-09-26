{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Monitor.Data.ListMetricValuesResponse
  ( ListMetricValuesResponse (..)
  -- Lenses
  , cost
  , interval
  , namespace
  , resourceregion
  , timespan
  , value
  ) where

import AzureExporter.Monitor.Contract (aesonOptions)
import AzureExporter.Monitor.Data.Metric (Metric)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- ListMetricValuesResponse
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response
data ListMetricValuesResponse =
  ListMetricValuesResponse { _cost           :: Int
                           , _interval       :: Text
                           , _namespace      :: Text
                           , _resourceregion :: Text
                           , _timespan       :: Text
                           , _value          :: [Metric]
                           } deriving (Generic, Show)

instance FromJSON ListMetricValuesResponse where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ListMetricValuesResponse
