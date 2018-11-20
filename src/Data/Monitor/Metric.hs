{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Monitor.Metric
  (
  -- * Types
    Metric (..)
  -- * Lenses
  , _id
  , _type
  , name
  , unit
  , timeseries
  ) where

import Data.Aeson.Options (aesonOptions)
import Data.Monitor.LocalizableString (LocalizableString)
import Data.Monitor.TimeSeriesElement (TimeSeriesElement)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- | Metric
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#metric>
data Metric =
  Metric { __id        :: Text
         , __type      :: Text
         , _name       :: LocalizableString
         , _unit       :: Text
         , _timeseries :: [TimeSeriesElement]
         } deriving (Generic, Show)

instance FromJSON Metric where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''Metric