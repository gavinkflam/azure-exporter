{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Prometheus.Gauge
    (
      -- * Types
      Gauge (..)
      -- * Lenses
    , name
    , help
    , labels
    , value
    , time
      -- * Output
    , renderGauges
    ) where

import Data.ByteString.Builder (Builder, byteString, string8)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8Builder)

import Control.Lens (makeLenses, (^.))
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)

import Text.Scientific (showFixed)
import Text.Time (formatTime)

-- | Data structure representing a gauge type metric.
--
--   <https://prometheus.io/docs/instrumenting/writing_exporters/#metrics>
data Gauge = Gauge
    { _name   :: {-# UNPACK #-} !Text
    , _help   :: {-# UNPACK #-} !Text
    , _labels :: [(Text, Text)]
    , _value  :: {-# UNPACK #-} !Scientific
    , _time   :: Maybe UTCTime
    } deriving (Eq, Show)

makeLenses ''Gauge

-- | Order gauges by time.
instance Ord Gauge where
    compare a b = compare (a ^. time) (b ^. time)

-- | Output gauges as metric exposition text.
--
-- @
-- # HELP metric_name Help message.
-- # TYPE metric_name gauge
-- metric_name{name1="value1",name2="value2"} 0.42
--
-- # HELP metric_name_2 Help message.
-- # TYPE metric_name_2 gauge
-- metric_name_2{name1="value1",name2="value2"} 0.31
-- @
renderGauges :: [Gauge] -> Builder
renderGauges gs = mconcat $ intersperse (byteString "\n\n") $ map renderGauge gs

-- | Output a gauge as metric exposition text.
--
-- @
-- # HELP metric_name Help message.
-- # TYPE metric_name gauge
-- metric_name{name1="value1",name2="value2"} 0.42
-- @
renderGauge :: Gauge -> Builder
renderGauge g = mconcat
    [ byteString "# HELP "
    , encodeUtf8Builder (g ^. name)
    , byteString " "
    , encodeUtf8Builder (g ^. help)
    , byteString "\n# TYPE "
    , encodeUtf8Builder (g ^. name)
    , byteString " gauge\n"
    , encodeUtf8Builder (g ^. name)
    , renderLabels (g ^. labels)
    , byteString " "
    , string8 $ showFixed (g ^. value)
    , maybe "" (string8 . formatTime " %s") (g ^. time)
    ]

-- | Output labels as metric exposition text.
--
-- @
-- {name1="value1",name2="value2"}
-- @
renderLabels :: [(Text, Text)] -> Builder
renderLabels [] = mempty
renderLabels ls = mconcat
    [ "{"
    , mconcat $ intersperse (byteString ",") $ map renderLabel ls
    , "}"
    ]

-- | Output a label as metric exposition text.
--
-- @
-- name="value"
-- @
renderLabel :: (Text, Text) -> Builder
renderLabel (n, v) = mconcat
    [ encodeUtf8Builder n
    , byteString "=\""
    , encodeUtf8Builder v
    , byteString "\""
    ]
