{-# LANGUAGE DeriveGeneric #-}

module Data.Monitor.TimeSeriesElement
    (
      -- * Types
      TimeSeriesElement (..)
    ) where

import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.MetricValue (MetricValue)

-- | TimeSeriesElement
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#timeserieselement>
newtype TimeSeriesElement = TimeSeriesElement
    { _data :: [MetricValue]
    } deriving (Generic, Show)

instance FromJSON TimeSeriesElement where
    parseJSON = genericParseJSON aesonOptions
