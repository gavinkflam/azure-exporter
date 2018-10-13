{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Data.Gauge
  (
  -- * Types
    Gauge (..)
  -- * Lenses
  , name
  , help
  , labels
  , value
  ) where

import Control.Lens (makeLenses)
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)

-- | Data structure representing a gauge type metric.
--
-- <https://prometheus.io/docs/instrumenting/writing_exporters/#metrics>
data Gauge =
  Gauge { _name   :: Text
        , _help   :: Text
        , _labels :: [(Text, Text)]
        , _value  :: Scientific
        } deriving (Eq, Show)

makeLenses ''Gauge
