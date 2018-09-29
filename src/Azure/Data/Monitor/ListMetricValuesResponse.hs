{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.Monitor.ListMetricValuesResponse
  ( ListMetricValuesResponse (..)
  -- Lenses
  , cost
  , interval
  , namespace
  , resourceregion
  , timespan
  , value
  ) where

import Azure.Contract (aesonOptions)
import Azure.Data.Monitor.Metric (Metric)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- ListMetricValuesResponse
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response
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