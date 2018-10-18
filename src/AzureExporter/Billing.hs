{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility to convert `GetRateCardResponse` and `Meter`s to `Gauge`s.
module AzureExporter.Billing
  (
  -- * Gauge
    gauges
  ) where

import           Data.Char (toLower)
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text.Lazy as T

import           Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import           Data.Scientific (Scientific)
import           Text.Casing (quietSnake)

import qualified Azure.Data.Billing.AggregateProperty as P
import qualified Azure.Data.Billing.GetRateCardResponse as R
import qualified Azure.Data.Billing.InstanceData as I
import qualified Azure.Data.Billing.Meter as M
import qualified Azure.Data.Billing.ResourceData as R
import qualified Azure.Data.Billing.UsageAggregate as U
import qualified AzureExporter.Data.Gauge as G
import qualified AzureExporter.Data.ResourceMetadata as D
import           AzureExporter.Util.Resource (parseResourceId)

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
                 , G._value  = quantity
                 , G._time   = Just endTime
                 }
  , costGauge <$> H.lookup (usage ^. U.properties ^. P.meterId) meters
  ]
    where namePrefix  = gaugeName usage
          labels      = gaugeLabels usage
          quantity    = usage ^. U.properties ^. P.quantity
          -- TODO: Resolve the real per-unit cost
          unitCost m  = head $ H.elems (m ^. M.meterRates)
          endTime     = usage ^. U.properties ^. P.usageEndTime
          costGauge m =
            G.Gauge { G._name   = namePrefix <> "_cost"
                    , G._help   = namePrefix <> "_cost"
                    , G._labels = labels
                    , G._value  = quantity * unitCost m
                    , G._time   = Just endTime
                    }

-- | Derive gauge name from `UsageAggregate`.
gaugeName :: U.UsageAggregate -> T.Text
gaugeName u =
  "azure_" <> T.intercalate "_" (map sanitizeName segments)
  where p        = u ^. U.properties
        segments = catMaybes [ p ^. P.meterCategory, p ^. P.meterName ]

-- | Derive gauge labels from `UsageAggregate`.
gaugeLabels :: U.UsageAggregate -> [(T.Text, T.Text)]
gaugeLabels u =
  [ ("meter_id",           p ^. P.meterId)
  , ("meter_category",     fromMaybe "Unknown" (p ^. P.meterCategory))
  , ("meter_sub_category", fromMaybe "N/A" (p ^. P.meterSubCategory))
  , ("meter_name",         fromMaybe "Unknown" (p ^. P.meterName))
  , ("meter_region",       fromMaybe "N/A" (p ^. P.meterRegion))
  , ("meter_unit",         fromMaybe "Unknown" (p ^. P.unit))
  , ("subscription_id",    p ^. P.subscriptionId)
  ]
  ++ gaugeLabelsFromInstanceData (p ^. P.instanceData)
    where p = u ^. U.properties

-- | Derive gauge labels from `InstanceData`.
gaugeLabelsFromInstanceData :: Maybe I.InstanceData -> [(T.Text, T.Text)]
gaugeLabelsFromInstanceData Nothing = []
gaugeLabelsFromInstanceData (Just i) =
  [ ("resource_id",       T.toLower $ r ^. R.resourceUri)
  , ("resource_region",   r ^. R.location)
  , ("resource_group",    d ^. D.resourceGroup)
  , ("resource_name",     d ^. D.resourceName)
  , ("resource_provider", d ^. D.resourceProvider)
  , ("resource_type",     d ^. D.resourceType)
  ]
  ++ gaugeLabelsFromResourceInfoMap "tags_" (r ^. R.tags)
  ++ gaugeLabelsFromResourceInfoMap "info_" (r ^. R.additionalInfo)
    where r = i ^. I.resourceData
          d = parseResourceId (r ^. R.resourceUri)

-- | Derive gauge labels from resource info map.
gaugeLabelsFromResourceInfoMap
  :: T.Text -> Maybe (H.HashMap T.Text (Maybe T.Text)) -> [(T.Text, T.Text)]
gaugeLabelsFromResourceInfoMap _ Nothing = []
gaugeLabelsFromResourceInfoMap prefix (Just m) =
  H.foldlWithKey' f [] m
    where f ts k v = (prefix <> k, maybe "" sanitize v) : ts
          sanitize = T.filter (not . flip elem illegalValueCharacters)

-- |
-- List of illegal characters to get rid of in the label values.
--
-- Tags and addition info values often contains new line characters for unknown
-- reasions
illegalValueCharacters :: String
illegalValueCharacters = "\r\n"

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
