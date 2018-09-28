{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.Monitor.MetricValue
  ( MetricValue (..)
  -- Lenses
  , average
  , count
  , maximum
  , minimum
  , timeStamp
  , total
  ) where

import Azure.Contract (aesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import GHC.Generics
import Prelude hiding (maximum, minimum)

-- MetricValue
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metricvalue
data MetricValue =
  MetricValue { _average   :: Maybe Scientific
              , _count     :: Maybe Scientific
              , _maximum   :: Maybe Scientific
              , _minimum   :: Maybe Scientific
              , _timeStamp :: Text
              , _total     :: Maybe Scientific
              } deriving (Generic, Show)

instance FromJSON MetricValue where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''MetricValue
