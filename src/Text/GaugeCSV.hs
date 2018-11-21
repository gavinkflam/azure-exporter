{-# LANGUAGE OverloadedStrings #-}

-- | Utility to convert `Gauge`s to `CSV`.
module Text.GaugeCSV
    (
      -- * Gauge
      toCSV
    ) where

import Data.Text (Text, pack)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import Control.Lens ((^.))
import qualified Data.HashMap.Strict as H

import qualified Data.CSV as C
import qualified Data.Gauge as G
import Text.Scientific (showFixed)

-- | Convert `Gauge`s to `CSV`.
toCSV :: [G.Gauge] -> C.CSV
toCSV =
    foldr f C.empty
  where
    f gauge csv = C.prependRow csv $ toRow gauge

-- | Convert `Gauge` to a `CSV` row.
toRow :: G.Gauge -> H.HashMap Text Text
toRow g =
    H.fromList $
        [ ("series", g ^. G.name)
        , ("value",  pack $ showFixed (g ^. G.value))
        ]
    ++ maybe []  fTimestamp (g ^. G.time)
    ++ map fLabel (g ^. G.labels)
  where
    fLabel (k, v)  = ("label_" <> k, v)
    showUTCTime    = filter (/= 's') . show . utcTimeToPOSIXSeconds
    fTimestamp t   = [("timestamp", pack $ showUTCTime t)]
