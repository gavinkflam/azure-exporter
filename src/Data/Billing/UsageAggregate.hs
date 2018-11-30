{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Data.Billing.UsageAggregate
    (
      -- * Types
      UsageAggregate (..)
      -- * Lenses
    , _id
    , name
    , properties
    , _type
      -- * Gauge
    , toGauges
    ) where

import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Control.Lens (makeLenses, (^.))
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
    { __id        :: {-# UNPACK #-} !Text
    , _name       :: {-# UNPACK #-} !Text
    , _properties :: {-# UNPACK #-} !Ap.AggregateProperty
    , __type      :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON UsageAggregate where
    parseJSON = genericParseJSON aesonOptions

makeLenses ''UsageAggregate


-- | Construct gauges from `GetRateCardResponse` and list of `UsageAggregate`.
toGauges :: Gr.GetRateCardResponse -> [UsageAggregate] -> [G.Gauge]
toGauges r =
    concatMap fGauges
  where
    meters  = indexMeters (r ^. Gr.meters)
    fGauges = usageAggregateToGauges (r ^. Gr.currency) meters

-- | Construct gauges from meter lookup map and individual `UsageAggregate`.
usageAggregateToGauges
    :: Text -> HashMap Text M.Meter -> UsageAggregate -> [G.Gauge]
usageAggregateToGauges currency meters usage = catMaybes
    [ Just usageGauge
    , costGauge <$> H.lookup (usage ^. (properties . Ap.meterId)) meters
    ]
  where
    namePrefix = gaugeNamePrefix usage
    baseLabels = baseGaugeLabels usage
    quantity   = usage ^. (properties . Ap.quantity)
    endTime    = usage ^. (properties . Ap.usageEndTime)
    usageGauge = G.Gauge
        { G._name   = namePrefix <> "_usage"
        , G._help   = namePrefix <> "_usage"
        , G._labels = baseLabels
        , G._value  = quantity
        , G._time   = Just endTime
        }
    costGauge m = G.Gauge
        { G._name   = namePrefix <> "_cost"
        , G._help   = namePrefix <> "_cost"
        , G._labels = baseLabels ++ costGaugeLabels currency m
        , G._value  = quantity * resolveUnitCost m
        , G._time   = Just endTime
        }

-- | Derive gauge name prefix from `UsageAggregate`.
gaugeNamePrefix :: UsageAggregate -> Text
gaugeNamePrefix u =
    Sn.metricName $ "azure_" <> T.intercalate "_" ns
  where
    p  = u ^. properties
    ns = catMaybes [p ^. Ap.meterCategory, p ^. Ap.meterName, p ^. Ap.unit]

-- | Derive base gauge labels from `UsageAggregate`.
baseGaugeLabels :: UsageAggregate -> [(Text, Text)]
baseGaugeLabels u =
    -- Static cloud provider label easing query composition
    [ ("cloud_provider",     "Azure")
    , ("meter_id",           p ^. Ap.meterId)
    , ("meter_category",     fromMaybe "Unknown" (p ^. Ap.meterCategory))
    , ("meter_sub_category", fromMaybe "N/A" (p ^. Ap.meterSubCategory))
    , ("meter_name",         fromMaybe "Unknown" (p ^. Ap.meterName))
    , ("meter_region",       fromMaybe "N/A" (p ^. Ap.meterRegion))
    , ("meter_unit",         fromMaybe "Unknown" (p ^. Ap.unit))
    , ("subscription_id",    p ^. Ap.subscriptionId)
    -- Time labels easing queries of aggregation
    , ("year",               T.pack $ formatTime "%Y" time)
    , ("month",              T.pack $ formatTime "%m" time)
    , ("year_month",         T.pack $ formatTime "%Y-%m" time)
    , ("date",               T.pack $ formatTime "%Y-%m-%d" time)
    ]
    ++ maybe [] instanceDataLabels (p ^. Ap.instanceData)
  where
    p    = u ^. properties
    time = p ^. Ap.usageEndTime

-- | Resolve the unit cost from `Meter`.
--
--   TODO: Resolve the accurate per-unit cost from usage quantity.
resolveUnitCost :: M.Meter -> Scientific
resolveUnitCost m = (m ^. M.meterRates) ! "0"

-- | Derive cost gauge specific labels from `UsageAggregate`.
costGaugeLabels :: Text -> M.Meter -> [(Text, Text)]
costGaugeLabels currency m =
    [ ("currency",  currency)
    , ("unit_cost", T.pack $ showFixed $ resolveUnitCost m)
    ]

-- | Derive gauge labels from `InstanceData`.
instanceDataLabels :: Id.InstanceData -> [(Text, Text)]
instanceDataLabels i =
    [ ("resource_id",       T.toLower $ r ^. Rd.resourceUri)
    , ("resource_region",   r ^. Rd.location)
    , ("resource_group",    d ^. Rm.resourceGroup)
    , ("resource_name",     d ^. Rm.resourceName)
    , ("resource_provider", d ^. Rm.resourceProvider)
    , ("resource_type",     d ^. Rm.resourceType)
    ]
    ++ maybe [] (resourceInfoLabels "tag_")  (r ^. Rd.tags)
    ++ maybe [] (resourceInfoLabels "info_") (r ^. Rd.additionalInfo)
  where
    r = i ^. Id.resourceData
    d = parseResourceId (r ^. Rd.resourceUri)

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
indexMeters = H.fromList . map (\m -> (m ^. M.meterId, m))
