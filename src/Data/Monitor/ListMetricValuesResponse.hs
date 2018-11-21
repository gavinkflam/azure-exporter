{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Monitor.ListMetricValuesResponse
    (
      -- * Types
      ListMetricValuesResponse (..)
      -- * Lenses
    , cost
    , interval
    , namespace
    , resourceregion
    , timespan
    , value
    ) where

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.Metric (Metric)

-- | Response for list metrics API.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response>
data ListMetricValuesResponse = ListMetricValuesResponse
    { _cost           :: {-# UNPACK #-} !Int
    , _interval       :: {-# UNPACK #-} !Text
    , _namespace      :: {-# UNPACK #-} !Text
    , _resourceregion :: {-# UNPACK #-} !Text
    , _timespan       :: {-# UNPACK #-} !Text
    , _value          :: [Metric]
    } deriving (Generic, Show)

instance FromJSON ListMetricValuesResponse where
    parseJSON = genericParseJSON aesonOptions

makeLenses ''ListMetricValuesResponse
