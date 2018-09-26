{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Monitor.Data.Metric
  ( Metric (..)
  -- Lenses
  , metricId
  , metricType
  , metricName
  , metricUnit
  , metricTimeseries
  ) where

import AzureExporter.Monitor.Contract (aesonOptions)
import AzureExporter.Monitor.Data.LocalizableString (LocalizableString)
import AzureExporter.Monitor.Data.TimeSeriesElement (TimeSeriesElement)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Text.Casing (camel)

-- Metric
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metric
data Metric =
  Metric { _metricId         :: Text
         , _metricType       :: Text
         , _metricName       :: LocalizableString
         , _metricUnit       :: Text
         , _metricTimeseries :: [TimeSeriesElement]
         } deriving (Generic, Show)

instance FromJSON Metric where
  parseJSON = genericParseJSON opts
    where opts = aesonOptions { fieldLabelModifier = camel . drop 7 }

makeLenses ''Metric
