{-# LANGUAGE OverloadedStrings #-}

-- | Utility to convert `ListMetricValuesResponse` to list of `Gauge`.
module Data.Monitor
    (
      -- * Gauge
      gauges
    ) where

import Data.Maybe (catMaybes)
import Data.Text.Lazy (Text, intercalate, pack, unpack)

import Control.Lens ((^.))
import Data.Scientific (Scientific)
import Text.Casing (quietSnake)

import qualified Data.Gauge as G
import qualified Data.Monitor.ListMetricValuesResponse as R
import qualified Data.Monitor.LocalizableString as LS
import qualified Data.Monitor.Metric as M
import qualified Data.Monitor.MetricValue as V
import qualified Data.Monitor.TimeSeriesElement as E
import Data.Resource (parseResourceId, resourceId)
import qualified Data.ResourceMetadata as D

-- | Extract information from `ListMetricValuesResponse` and construct the
--   corresponding list of `Gauge`.
gauges :: R.ListMetricValuesResponse -> [G.Gauge]
gauges r =
    concatMap fGauges metrics
  where
    fGauges = gaugesFromMetric (r ^. R.resourceregion)
    metrics = r ^. R.value

-- | Extract information from `Metric` and construct the
--   corresponding list of `Gauge`.
gaugesFromMetric :: Text -> M.Metric -> [G.Gauge]
gaugesFromMetric region metric =
    concatMap fGauges values
  where
    metadata   = parseResourceId (metric ^. M._id)
    namePrefix =
        joinNameSegments
        [ "azure"
        , metadata ^. D.resourceType
        , metric ^. (M.name . LS.value)
        , metric ^. M.unit
        ]
    labels     = deriveLabels region metadata metric
    fGauges    = gaugesFromMetricValue namePrefix labels
    values     = concatMap (^. E._data) (metric ^. M.timeseries)

-- | Extract information from `MetricValue` and construct the
--   corresponding list of `Gauge`.
gaugesFromMetricValue :: Text -> [(Text, Text)] -> V.MetricValue -> [G.Gauge]
gaugesFromMetricValue namePrefix labels value =
    catMaybes
    [ fGauge "average" (value ^. V.average)
    , fGauge "count"   (value ^. V.count)
    , fGauge "maximum" (value ^. V.maximum)
    , fGauge "minimum" (value ^. V.minimum)
    , fGauge "total"   (value ^. V.total)
    ]
  where
    fName  n = namePrefix <> "_" <> quietSnakeT n
    fGauge n = gaugeFromAggregation (fName n) labels

-- | Construct the corresponding list of `Gauge` for a individual aggregation.
gaugeFromAggregation :: Text -> [(Text, Text)] -> Maybe Scientific -> Maybe G.Gauge
gaugeFromAggregation _ _ Nothing          = Nothing
gaugeFromAggregation name labels (Just n) = Just G.Gauge
    { G._name   = name
    , G._help   = name
    , G._labels = labels
    , G._value  = n
    , G._time   = Nothing
    }

-- | Derive labels from resource region, `ResourceMetadata` and `Metric`.
deriveLabels :: Text -> D.ResourceMetadata -> M.Metric -> [(Text, Text)]
deriveLabels resourceRegion metadata metric =
    [ ("resource_group",    metadata ^. D.resourceGroup)
    , ("resource_id",       resourceId (metric ^. M._id))
    , ("resource_name",     metadata ^. D.resourceName)
    , ("resource_provider", metadata ^. D.resourceProvider)
    , ("resource_region",   resourceRegion)
    , ("resource_type",     metadata ^. D.resourceType)
    , ("subscription_id",   metadata ^. D.subscriptionId)
    ]

-- | Join the name segments in underscore delimited lower case manner.
joinNameSegments :: [Text] -> Text
joinNameSegments s = intercalate "_" $ map quietSnakeT s

-- | Convert content in a `Text` into underscore delimited lower case manner.
quietSnakeT :: Text -> Text
quietSnakeT = pack . quietSnake . unpack
