{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility to convert `Gauge`s to `CSV`.
module AzureExporter.Text.GaugeCSV
  (
  -- * Gauge
    toCSV
  ) where

import           Data.Text.Lazy (Text, pack)

import           Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import           Data.Scientific (FPFormat(Fixed), formatScientific)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)

import qualified AzureExporter.Data.CSV as C
import qualified AzureExporter.Data.Gauge as G

-- | Convert `Gauge`s to `CSV`.
toCSV :: [G.Gauge] -> C.CSV
toCSV = foldl f C.empty
  where f csv gauge = C.prependRow csv $ toRow gauge

-- | Convert `Gauge` to a `CSV` row.
toRow :: G.Gauge -> H.HashMap Text Text
toRow g =
  H.fromList $
    [ ("series", g ^. G.name)
    , ("value",  pack $ showScientific (g ^. G.value))
    ]
    ++ maybe []  fTimestamp (g ^. G.time)
    ++ map fLabel (g ^. G.labels)
      where fLabel (k, v)  = ("label_" <> k, v)
            showScientific = formatScientific Fixed Nothing
            showUTCTime    = filter (/= 's') . show . utcTimeToPOSIXSeconds
            fTimestamp t   = [("timestamp", pack $ showUTCTime t)]
