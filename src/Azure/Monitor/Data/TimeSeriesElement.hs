{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Monitor.Data.TimeSeriesElement
  ( TimeSeriesElement (..)
  -- Lenses
  , elementData
  ) where

import Azure.Monitor.Contract (aesonOptions)
import Azure.Monitor.Data.MetricValue (MetricValue)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Text.Casing (camel)

-- TimeSeriesElement
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#timeserieselement
data TimeSeriesElement =
  TimeSeriesElement { _elementData :: [MetricValue]
                    } deriving (Generic, Show)

instance FromJSON TimeSeriesElement where
  parseJSON = genericParseJSON opts
    where opts = aesonOptions { fieldLabelModifier = camel . drop 8 }

makeLenses ''TimeSeriesElement
