{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Monitor.Data.Metric
  ( Metric (..)
  -- Lenses
  , _id
  , _type
  , name
  , unit
  , timeseries
  ) where

import Azure.Monitor.Contract (aesonOptions)
import Azure.Monitor.Data.LocalizableString (LocalizableString)
import Azure.Monitor.Data.TimeSeriesElement (TimeSeriesElement)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- Metric
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metric
data Metric =
  Metric { __id         :: Text
         , __type       :: Text
         , _name       :: LocalizableString
         , _unit       :: Text
         , _timeseries :: [TimeSeriesElement]
         } deriving (Generic, Show)

instance FromJSON Metric where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''Metric
