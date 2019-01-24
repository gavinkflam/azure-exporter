{-# LANGUAGE DeriveGeneric #-}

module Data.Monitor.MetricValue
    (
      -- * Types
      MetricValue (..)
    ) where

import Data.Text (Text)
import GHC.Generics
import Prelude hiding (maximum, minimum)

import Data.Aeson
import Data.Scientific (Scientific)

import Data.Aeson.Options (aesonOptions)

-- | MetricValue
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metricvalue>
data MetricValue = MetricValue
    { average   :: Maybe Scientific
    , count     :: Maybe Scientific
    , maximum   :: Maybe Scientific
    , minimum   :: Maybe Scientific
    , timeStamp :: {-# UNPACK #-} !Text
    , total     :: Maybe Scientific
    } deriving (Generic, Show)

instance FromJSON MetricValue where
    parseJSON = genericParseJSON aesonOptions
