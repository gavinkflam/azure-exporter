{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

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
import Data.Text.Encoding (encodeUtf8)

import Control.Lens (makeLenses, (^.))
import Data.Csv
    ( DefaultOrdered
    , ToNamedRecord
    , headerOrder
    , namedRecord
    , toNamedRecord
    , (.=)
    )
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)
import Data.Vector (fromList)

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

-- | Convert `Gauge` to a CSV record.
instance ToNamedRecord Gauge where
    toNamedRecord g = namedRecord $
        [ "series"    .= (g ^. name)
        , "value"     .= showFixed (g ^. value)
        , "timestamp" .= fmap (formatTime "%s") (g ^. time)
        ]
        ++ map fLabel (g ^. labels)
      where
        fLabel (k, v)  = encodeUtf8 ("label_" <> k) .= v

-- | Extract the CSV column headers from a `Gauge`.
--
--   'series', 'value' and 'timestamp' columns should go first.
--   Label names were prefixed with 'label_' and come next in alphabetical order.
instance DefaultOrdered Gauge where
    headerOrder g =
        fromList $ ["series", "value", "timestamp"] ++ map fName (g ^. labels)
      where
        fName (k, _) = encodeUtf8 $ "label_" <> k

-- | Order `Gauge` by time.
instance Ord Gauge where
    compare a b = compare (a ^. time) (b ^. time)
