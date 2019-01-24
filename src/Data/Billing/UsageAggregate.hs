{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Billing.UsageAggregate
    (
      -- * Types
      UsageAggregate (..)
      -- * Gauge
    , toGauges
    ) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Data.Aeson
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as H
import Data.Scientific (Scientific)

import Data.Aeson.Options (aesonOptions)
import qualified Data.AzureRm.ResourceMetadata as Rm
import qualified Data.Billing.AggregateProperty as Ap
import qualified Data.Billing.GetRateCardResponse as Gr
import qualified Data.Billing.InstanceData as Id
import qualified Data.Billing.Meter as M
import qualified Data.Billing.ResourceData as Rd
import qualified Data.Prometheus.Gauge as G
import Text.AzureRm.Resource (parseResourceId)
import qualified Text.Prometheus.Sanitize as Sn
import Text.Scientific (showFixed)
import Text.Time (formatTime)

-- | UsageAggregate
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data UsageAggregate = UsageAggregate
    { _id        :: {-# UNPACK #-} !Text
    , name       :: {-# UNPACK #-} !Text
    , properties :: {-# UNPACK #-} !Ap.AggregateProperty
    , _type      :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON UsageAggregate where
    parseJSON = genericParseJSON aesonOptions

-- | Construct gauges from `GetRateCardResponse` and list of `UsageAggregate`.
toGauges :: Gr.GetRateCardResponse -> [UsageAggregate] -> [G.Gauge]
toGauges r =
    concatMap fGauges
  where
    meters  = indexMeters $ Gr.meters r
    fGauges = usageAggregateToGauges (Gr.currency r) meters

-- | Construct gauges from meter lookup map and individual `UsageAggregate`.
usageAggregateToGauges
    :: Text -> HashMap Text M.Meter -> UsageAggregate -> [G.Gauge]
usageAggregateToGauges currency meters usage = catMaybes
    [ Just usageGauge
    , costGauge <$> H.lookup (Ap.meterId $ properties usage) meters
    ]
  where
    namePrefix = gaugeNamePrefix usage
    baseLabels = baseGaugeLabels usage
    quantity   = Ap.quantity $ properties usage
    startTime  = Ap.usageStartTime $ properties usage
    usageGauge = G.Gauge
        { G.name   = namePrefix <> "_usage"
        , G.help   = namePrefix <> "_usage"
        , G.labels = baseLabels
        , G.value  = quantity
        , G.time   = Just startTime
        }
    costGauge m = G.Gauge
        { G.name   = namePrefix <> "_cost"
        , G.help   = namePrefix <> "_cost"
        , G.labels = baseLabels ++ costGaugeLabels currency m
        , G.value  = quantity * resolveUnitCost m
        , G.time   = Just startTime
        }

-- | Derive gauge name prefix from `UsageAggregate`.
gaugeNamePrefix :: UsageAggregate -> Text
gaugeNamePrefix u =
    Sn.metricName $ "azure_" <> T.intercalate "_" ns
  where
    p  = properties u
    ns = catMaybes [Ap.meterCategory p, Ap.meterName p, Ap.unit p]

-- | Derive base gauge labels from `UsageAggregate`.
baseGaugeLabels :: UsageAggregate -> [(Text, Text)]
baseGaugeLabels u =
    -- Static cloud provider label easing query composition
    [ ("cloud_provider",     "Azure")
    , ("meter_id",           Ap.meterId p)
    , ("meter_category",     fromMaybe "Unknown" $ Ap.meterCategory p)
    , ("meter_sub_category", fromMaybe "N/A" $ Ap.meterSubCategory p)
    , ("meter_name",         fromMaybe "Unknown" $ Ap.meterName p)
    , ("meter_region",       fromMaybe "N/A" $ Ap.meterRegion p)
    , ("meter_unit",         fromMaybe "Unknown" $ Ap.unit p)
    , ("subscription_id",    Ap.subscriptionId p)
    -- Time labels easing queries of aggregation
    , ("year",               T.pack $ formatTime "%Y" time)
    , ("month",              T.pack $ formatTime "%m" time)
    , ("year_month",         T.pack $ formatTime "%Y-%m" time)
    , ("date",               T.pack $ formatTime "%Y-%m-%d" time)
    ]
    ++ maybe [] instanceDataLabels (Ap.instanceData p)
  where
    p    = properties u
    time = Ap.usageStartTime p

-- | Resolve the unit cost from `Meter`.
--
--   TODO: Resolve the accurate per-unit cost from usage quantity.
resolveUnitCost :: M.Meter -> Scientific
resolveUnitCost m = M.meterRates m ! "0"

-- | Derive cost gauge specific labels from `UsageAggregate`.
costGaugeLabels :: Text -> M.Meter -> [(Text, Text)]
costGaugeLabels currency m =
    [ ("currency",  currency)
    , ("unit_cost", T.pack $ showFixed $ resolveUnitCost m)
    ]

-- | Derive gauge labels from `InstanceData`.
instanceDataLabels :: Id.InstanceData -> [(Text, Text)]
instanceDataLabels i =
    [ ("resource_id",       T.toLower $ Rd.resourceUri r)
    , ("resource_region",   Rd.location r)
    , ("resource_group",    Rm.resourceGroup d)
    , ("resource_name",     Rm.resourceName d)
    , ("resource_provider", Rm.resourceProvider d)
    , ("resource_type",     Rm.resourceType d)
    ]
    ++ maybe [] (resourceInfoLabels "tag_")  (Rd.tags r)
    ++ maybe [] (resourceInfoLabels "info_") (Rd.additionalInfo r)
  where
    r = Id.resourceData i
    d = parseResourceId $ Rd.resourceUri r

-- | Derive gauge labels from resource info map.
--
--   Label name and value will be sanitized.
--   Labels with empty or null value will be removed.
--
--   TODO: Remove labels with duplicated name.
resourceInfoLabels
    :: Text -> HashMap Text (Maybe Text) -> [(Text, Text)]
resourceInfoLabels n m =
    filter ((/="") . snd) $ H.foldlWithKey' f [] m
  where
    f z k v = (n <> Sn.sanitizeName k, maybe "" Sn.sanitizeLabelValue v) : z

-- | Construct a meters lookup map indexed by meter ID for efficient searching.
indexMeters :: [M.Meter] -> HashMap Text M.Meter
indexMeters = H.fromList . map (\m -> (M.meterId m, m))
