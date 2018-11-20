{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Monitor.TimeSeriesElement
  (
  -- * Types
    TimeSeriesElement (..)
  -- * Lenses
  , _data
  ) where

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.MetricValue (MetricValue)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- | TimeSeriesElement
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#timeserieselement>
newtype TimeSeriesElement =
  TimeSeriesElement { __data :: [MetricValue]
                    } deriving (Generic, Show)

instance FromJSON TimeSeriesElement where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''TimeSeriesElement
