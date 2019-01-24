{-# LANGUAGE DeriveGeneric #-}

module Data.Monitor.Metric
    (
    -- * Types
      Metric (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.LocalizableString (LocalizableString)
import Data.Monitor.TimeSeriesElement (TimeSeriesElement)

-- | Metric
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metric>
data Metric = Metric
    { _id        :: {-# UNPACK #-} !Text
    , _type      :: {-# UNPACK #-} !Text
    , name       :: {-# UNPACK #-} !LocalizableString
    , unit       :: {-# UNPACK #-} !Text
    , timeseries :: [TimeSeriesElement]
    } deriving (Generic, Show)

instance FromJSON Metric where
    parseJSON = genericParseJSON aesonOptions
