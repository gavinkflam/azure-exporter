{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

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

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.Metric (Metric)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- | Response for list metrics API.
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response>
data ListMetricValuesResponse =
  ListMetricValuesResponse { _cost           :: Int
                           , _interval       :: Text
                           , _namespace      :: Text
                           , _resourceregion :: Text
                           , _timespan       :: Text
                           , _value          :: [Metric]
                           } deriving (Generic, Show)

instance FromJSON ListMetricValuesResponse where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ListMetricValuesResponse