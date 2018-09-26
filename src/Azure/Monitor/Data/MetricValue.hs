{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Monitor.Data.MetricValue
  ( MetricValue (..)
  -- Lenses
  , average
  , count
  , maximum
  , minimum
  , timeStamp
  , total
  ) where

import Azure.Monitor.Contract (aesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Ratio (Rational)
import Data.Text (Text)
import GHC.Generics
import Prelude hiding (maximum, minimum)

-- MetricValue
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metricvalue
data MetricValue =
  MetricValue { _average   :: Rational
              , _count     :: Int
              , _maximum   :: Rational
              , _minimum   :: Rational
              , _timeStamp :: Text
              , _total     :: Rational
              } deriving (Generic, Show)

instance FromJSON MetricValue where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''MetricValue
