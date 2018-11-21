{-# LANGUAGE OverloadedStrings #-}

-- | Utility to convert `GetRateCardResponse` and `Meter`s to `Gauge`s.
module Data.Billing
    (
      -- * Gauge
      gauges
    ) where

import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text.Lazy as T

import Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import Data.Scientific (Scientific)

import qualified Data.Billing.AggregateProperty as P
import qualified Data.Billing.GetRateCardResponse as R
import qualified Data.Billing.InstanceData as I
import qualified Data.Billing.Meter as M
import qualified Data.Billing.ResourceData as R
import qualified Data.Billing.UsageAggregate as U
import qualified Data.Gauge as G
import Data.Prometheus (metricName, sanitizeLabelValue, sanitizeName)
import Data.Resource (parseResourceId)
import qualified Data.ResourceMetadata as D
import Text.Scientific (showFixed)
import Text.Time (formatTime)

-- | Extract information from `GetRateCardResponse` and `Meter`s to construct
--   the corresponding `Gauge`s.
gauges :: R.GetRateCardResponse -> [U.UsageAggregate] -> [G.Gauge]
gauges r =
    concatMap $ gaugesFromUsageAggregate (r ^. R.currency) meters
  where
    meters = indexMeters (r ^. R.meters)

-- | Extract information from `UsageAggregate` and `Meter`s to construct the
--   corresponding `Gauge`s.
gaugesFromUsageAggregate
    :: T.Text -> H.HashMap T.Text M.Meter -> U.UsageAggregate -> [G.Gauge]
gaugesFromUsageAggregate currency meters usage =
    catMaybes
    [ Just G.Gauge
        { G._name   = name <> "_usage"
        , G._help   = name <> "_usage"
        , G._labels = labels
        , G._value  = quantity
        , G._time   = Just endTime
        }
    , costGauge <$> H.lookup (usage ^. (U.properties . P.meterId)) meters
    ]
  where
    name        = gaugeName usage
    labels      = gaugeLabels usage
    quantity    = usage ^. (U.properties . P.quantity)
    endTime     = usage ^. (U.properties . P.usageEndTime)
    costGauge m = G.Gauge
        { G._name   = name <> "_cost"
        , G._help   = name <> "_cost"
        , G._labels = labels ++ costGaugeLabels currency m
        , G._value  = quantity * resolveUnitCost m
        , G._time   = Just endTime
        }

-- | Derive gauge name from `UsageAggregate`.
gaugeName :: U.UsageAggregate -> T.Text
gaugeName u =
    metricName $ "azure_" <> T.intercalate "_" ns
  where
    p  = u ^. U.properties
    ns = catMaybes [p ^. P.meterCategory, p ^. P.meterName, p ^. P.unit]

-- | Derive gauge labels from `UsageAggregate`.
gaugeLabels :: U.UsageAggregate -> [(T.Text, T.Text)]
gaugeLabels u =
    -- Static cloud provider label easing query composition
    [ ("cloud_provider",     "Azure")
    , ("meter_id",           p ^. P.meterId)
    , ("meter_category",     fromMaybe "Unknown" (p ^. P.meterCategory))
    , ("meter_sub_category", fromMaybe "N/A" (p ^. P.meterSubCategory))
    , ("meter_name",         fromMaybe "Unknown" (p ^. P.meterName))
    , ("meter_region",       fromMaybe "N/A" (p ^. P.meterRegion))
    , ("meter_unit",         fromMaybe "Unknown" (p ^. P.unit))
    , ("subscription_id",    p ^. P.subscriptionId)
    -- Time labels easing queries of aggregation
    , ("year",               T.pack $ formatTime "%Y" time)
    , ("month",              T.pack $ formatTime "%m" time)
    , ("year_month",         T.pack $ formatTime "%Y-%m" time)
    , ("date",               T.pack $ formatTime "%Y-%m-%d" time)
    ]
    ++ maybe [] gaugeLabelsFromInstanceData (p ^. P.instanceData)
  where
    p    = u ^. U.properties
    time = p ^. P.usageEndTime

-- | Resolve the unit cost from `Meter`.
--
--   TODO: Resolve the accurate per-unit cost from usage quantity.
resolveUnitCost :: M.Meter -> Scientific
resolveUnitCost m = (m ^. M.meterRates) H.! "0"

-- | Derive cost gauge specific labels from `UsageAggregate`.
costGaugeLabels :: T.Text -> M.Meter -> [(T.Text, T.Text)]
costGaugeLabels currency m =
    [ ("currency",  currency)
    , ("unit_cost", T.pack $ showFixed $ resolveUnitCost m)
    ]

-- | Derive gauge labels from `InstanceData`.
gaugeLabelsFromInstanceData :: I.InstanceData -> [(T.Text, T.Text)]
gaugeLabelsFromInstanceData i =
    [ ("resource_id",       T.toLower $ r ^. R.resourceUri)
    , ("resource_region",   r ^. R.location)
    , ("resource_group",    d ^. D.resourceGroup)
    , ("resource_name",     d ^. D.resourceName)
    , ("resource_provider", d ^. D.resourceProvider)
    , ("resource_type",     d ^. D.resourceType)
    ]
    ++ maybe [] (gaugeLabelsFromResourceInfoMap "tag_")  (r ^. R.tags)
    ++ maybe [] (gaugeLabelsFromResourceInfoMap "info_") (r ^. R.additionalInfo)
  where
    r = i ^. I.resourceData
    d = parseResourceId (r ^. R.resourceUri)

-- | Derive gauge labels from resource info map.
--
--   Label name and value will be sanitized.
--   Labels with empty or null value will be removed.
--
--   TODO: Remove labels with duplicated name.
gaugeLabelsFromResourceInfoMap
    :: T.Text -> H.HashMap T.Text (Maybe T.Text) -> [(T.Text, T.Text)]
gaugeLabelsFromResourceInfoMap n m =
    filter ((/="") . snd) $ H.foldlWithKey' f [] m
  where
    f z k v = (n <> sanitizeName k, maybe "" sanitizeLabelValue v) : z

-- | Construct a `Meter`s lookup `HashMap` indexed with meter ID for efficient
--   searching.
indexMeters :: [M.Meter] -> H.HashMap T.Text M.Meter
indexMeters = H.fromList . map (\m -> (m ^. M.meterId, m))
