{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility to convert `GetRateCardResponse` and `Meter`s to `Gauge`s.
module AzureExporter.Billing
  (
  -- * Gauge
    gauges
  ) where

import           Data.Char (toLower)
import           Data.Maybe (catMaybes)
import qualified Data.Text.Lazy as T

import           Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import           Data.Scientific (Scientific)
import           Text.Casing (quietSnake)

import qualified Azure.Data.Billing.AggregateProperty as P
import qualified Azure.Data.Billing.GetRateCardResponse as R
import qualified Azure.Data.Billing.Meter as M
import qualified Azure.Data.Billing.UsageAggregate as U
import qualified AzureExporter.Data.Gauge as G
import qualified AzureExporter.Data.ResourceMetadata as D
import           AzureExporter.Util.Resource (parseResourceId, resourceId)

-- |
-- Extract information from `GetRateCardResponse` and `Meter`s to construct the
-- corresponding `Gauge`s.
gauges :: R.GetRateCardResponse -> [U.UsageAggregate] -> [G.Gauge]
gauges r = concatMap $ gaugesFromUsageAggregate (r ^. R.currency) meters
  where meters = indexMeters (r ^. R.meters)

-- |
-- Extract information from `UsageAggregate` and `Meter`s to construct the
-- corresponding `Gauge`s.
gaugesFromUsageAggregate
  :: T.Text -> H.HashMap T.Text M.Meter -> U.UsageAggregate -> [G.Gauge]
gaugesFromUsageAggregate currency meters usage =
  catMaybes
  [ Just G.Gauge { G._name   = namePrefix <> "_usage"
                 , G._help   = namePrefix <> "_usage"
                 , G._labels = labels
                 , G._value  = usage ^. U.properties ^. P.quantity
                 }
  , costGauge <$> H.lookup (usage ^. U.properties ^. P.meterId) meters
  ]
    where namePrefix  = gaugeName usage
          labels      = gaugeLabels usage
          costGauge m =
            G.Gauge { G._name   = namePrefix <> "_cost"
                    , G._help   = namePrefix <> "_cost"
                    , G._labels = labels
                    , G._value  = head $ H.elems (m ^. M.meterRates)
                    }

-- | Derive gauge name from `UsageAggregate`.
gaugeName :: U.UsageAggregate -> T.Text
gaugeName u =
  "azure_" <> T.intercalate "_" (map sanitizeName segments)
  where p        = u ^. U.properties
        segments = catMaybes [ p ^. P.meterCategory, p ^. P.meterName ]

-- | Derive gauge labels from `UsageAggregate`.
gaugeLabels :: U.UsageAggregate -> [(T.Text, T.Text)]
gaugeLabels u = []

-- |
-- Sanitize and standardize gauge name.
--
-- The following steps will be done.
--
-- 1. Replace special characters with underscore.
-- 2. Downcase alphabetical characters.
-- 3. Remove consecutive underscores.
sanitizeName :: T.Text -> T.Text
sanitizeName n =
  T.intercalate "_" $ filter (/= T.empty) $ T.splitOn "_" $ T.map f n
    where f c = if c `elem` specialCharacters then '_' else toLower c

-- | List of special characters to get rid of in the gauge name.
specialCharacters :: String
specialCharacters = "-()/\\ "

-- |
-- Construct a `Meter`s lookup `HashMap` indexed with meter ID for efficient
-- searching.
indexMeters :: [M.Meter] -> H.HashMap T.Text M.Meter
indexMeters ms = H.fromList $ map item ms
  where item m = (m ^. M.meterId, m)
