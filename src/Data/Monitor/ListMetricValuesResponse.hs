{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Monitor.ListMetricValuesResponse
    (
      -- * Types
      ListMetricValuesResponse (..)
      -- * Gauges
    , toGauges
    ) where

import Data.Maybe (catMaybes)
import Data.Text (Text, intercalate, pack, unpack)
import GHC.Generics

import Data.Aeson
import Data.Scientific (Scientific)
import Text.Casing (quietSnake)

import Data.Aeson.Options (aesonOptions)
import qualified Data.AzureRm.ResourceMetadata as D
import qualified Data.Monitor.LocalizableString as LS
import qualified Data.Monitor.Metric as M
import qualified Data.Monitor.MetricValue as V
import qualified Data.Monitor.TimeSeriesElement as E
import qualified Data.Prometheus.Gauge as G
import Text.AzureRm.Resource (parseResourceId, resourceId)

-- | Response for list metrics API.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response>
data ListMetricValuesResponse = ListMetricValuesResponse
    { cost           :: {-# UNPACK #-} !Int
    , interval       :: {-# UNPACK #-} !Text
    , namespace      :: {-# UNPACK #-} !Text
    , resourceregion :: {-# UNPACK #-} !Text
    , timespan       :: {-# UNPACK #-} !Text
    , value          :: [M.Metric]
    } deriving (Generic, Show)

instance FromJSON ListMetricValuesResponse where
    parseJSON = genericParseJSON aesonOptions

-- | Construct gauges from `ListMetricValuesResponse`.
toGauges :: ListMetricValuesResponse -> [G.Gauge]
toGauges r =
    concatMap fGauges $ value r
  where
    fGauges = metricToGauges $ resourceregion r

-- | Construct gauges from `Metric`.
metricToGauges :: Text -> M.Metric -> [G.Gauge]
metricToGauges resourceRegion metric =
    concatMap fGauges values
  where
    metadata   = parseResourceId $ M._id metric
    namePrefix = intercalate "_" $ map quietSnakeT
        [ "azure"
        , D.resourceType metadata
        , LS.value $ M.name metric
        , M.unit metric
        ]
    labels     = deriveLabels resourceRegion metadata metric
    fGauges    = metricValueToGauges namePrefix labels
    values     = concatMap E._data $ M.timeseries metric

-- | Construct gauges from `MetricValue`.
metricValueToGauges :: Text -> [(Text, Text)] -> V.MetricValue -> [G.Gauge]
metricValueToGauges namePrefix labels metricValue = catMaybes
    [ fGauge "average" $ V.average metricValue
    , fGauge "count"   $ V.count metricValue
    , fGauge "maximum" $ V.maximum metricValue
    , fGauge "minimum" $ V.minimum metricValue
    , fGauge "total"   $ V.total metricValue
    ]
  where
    fName  n = namePrefix <> "_" <> quietSnakeT n
    fGauge n = aggregationToGauges (fName n) labels

-- | Construct gauges from a individual aggregation.
aggregationToGauges
    :: Text -> [(Text, Text)] -> Maybe Scientific -> Maybe G.Gauge
aggregationToGauges _ _ Nothing          = Nothing
aggregationToGauges name labels (Just n) = Just G.Gauge
    { G.name   = name
    , G.help   = name
    , G.labels = labels
    , G.value  = n
    , G.time   = Nothing
    }

-- | Derive labels from `Metric`.
deriveLabels :: Text -> D.ResourceMetadata -> M.Metric -> [(Text, Text)]
deriveLabels resourceRegion metadata metric =
    [ ("resource_group",    D.resourceGroup metadata)
    , ("resource_id",       resourceId $ M._id metric)
    , ("resource_name",     D.resourceName metadata)
    , ("resource_provider", D.resourceProvider metadata)
    , ("resource_region",   resourceRegion)
    , ("resource_type",     D.resourceType metadata)
    , ("subscription_id",   D.subscriptionId metadata)
    ]

-- | Convert text into underscore delimited lower case manner.
quietSnakeT :: Text -> Text
quietSnakeT = pack . quietSnake . unpack
