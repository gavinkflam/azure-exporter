{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Monitor.Data.TimeSeriesElement
  ( TimeSeriesElement (..)
  -- Lenses
  , _data
  ) where

import Azure.Monitor.Contract (aesonOptions)
import Azure.Monitor.Data.MetricValue (MetricValue)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- TimeSeriesElement
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#timeserieselement
data TimeSeriesElement =
  TimeSeriesElement { __data :: [MetricValue]
                    } deriving (Generic, Show)

instance FromJSON TimeSeriesElement where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''TimeSeriesElement
