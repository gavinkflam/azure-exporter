{-# LANGUAGE OverloadedStrings #-}

module AzureExporter.Monitor
  ( gauges
  ) where

import qualified Azure.Data.Monitor.ListMetricValuesResponse as R
import qualified Azure.Data.Monitor.LocalizableString as LS
import qualified Azure.Data.Monitor.Metric as M
import qualified Azure.Data.Monitor.MetricValue as V
import qualified Azure.Data.Monitor.TimeSeriesElement as E
import qualified AzureExporter.Data.Gauge as G
import qualified AzureExporter.Data.ResourceMetadata as D
import           AzureExporter.Util.Resource (parseResourceId, resourceId)
import           Control.Lens ((^.))
import           Data.Scientific (Scientific)
import           Data.Maybe (catMaybes)
import           Data.Text.Lazy (Text, intercalate, pack, unpack)
import           Text.Casing (quietSnake)

gauges :: R.ListMetricValuesResponse -> [G.Gauge]
gauges r = concatMap fGauges metrics
  where fGauges = gaugesFromMetric $ r ^. R.resourceregion
        metrics = r ^. R.value

gaugesFromMetric :: Text -> M.Metric -> [G.Gauge]
gaugesFromMetric region metric = concatMap fGauges values
  where metadata   = parseResourceId $ metric ^. M._id
        namePrefix = joinNameSegments [ "azure"
                                      , metadata ^. D.resourceType
                                      , metric ^. M.name ^. LS.value
                                      , metric ^. M.unit
                                      ]
        labels     = deriveLabels region metadata metric
        fGauges    = gaugesFromMetricValue namePrefix labels
        values     = concatMap (^. E._data) $ metric ^. M.timeseries

gaugesFromMetricValue :: Text -> [(Text, Text)] -> V.MetricValue -> [G.Gauge]
gaugesFromMetricValue namePrefix labels value =
  catMaybes [ fGauge "average" $ value ^. V.average
            , fGauge "count"   $ value ^. V.count
            , fGauge "maximum" $ value ^. V.maximum
            , fGauge "minimum" $ value ^. V.minimum
            , fGauge "total"   $ value ^. V.total
            ]
              where fName  n = namePrefix <> "_" <> quietSnakeT n
                    fGauge n = gaugeFromAggregation (fName n) labels

gaugeFromAggregation :: Text -> [(Text, Text)] -> Maybe Scientific -> Maybe G.Gauge
gaugeFromAggregation _ _ Nothing          = Nothing
gaugeFromAggregation name labels (Just n) =
  Just G.Gauge { G._name   = name
               , G._help   = name
               , G._labels = labels
               , G._value  = n
               }

-- Utilities
deriveLabels :: Text -> D.ResourceMetadata -> M.Metric -> [(Text, Text)]
deriveLabels resourceRegion metadata metric =
  [ ("resource_group",    metadata ^. D.resourceGroup)
  , ("resource_id",       resourceId $ metric ^. M._id)
  , ("resource_name",     metadata ^. D.resourceName)
  , ("resource_provider", metadata ^. D.resourceProvider)
  , ("resource_region",   resourceRegion)
  , ("resource_type",     metadata ^. D.resourceType)
  , ("subscription_id",   metadata ^. D.subscriptionId)
  ]

joinNameSegments :: [Text] -> Text
joinNameSegments s = intercalate "_" $ map quietSnakeT s

quietSnakeT :: Text -> Text
quietSnakeT = pack . quietSnake . unpack
