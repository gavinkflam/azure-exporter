{-# LANGUAGE TemplateHaskell #-}

module Data.Gauge
    (
      -- * Types
      Gauge (..)
      -- * Lenses
    , name
    , help
    , labels
    , value
    , time
    ) where

import Data.Text (Text)

import Control.Lens (makeLenses, (^.))
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)

-- | Data structure representing a gauge type metric.
--
--   <https://prometheus.io/docs/instrumenting/writing_exporters/#metrics>
data Gauge = Gauge
    { _name   :: Text
    , _help   :: Text
    , _labels :: [(Text, Text)]
    , _value  :: Scientific
    , _time   :: Maybe UTCTime
    } deriving (Eq, Show)

makeLenses ''Gauge

-- | Order `Gauge` by time.
instance Ord Gauge where
    compare a b = compare (a ^. time) (b ^. time)
