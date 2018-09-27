{-# LANGUAGE OverloadedStrings #-}

module AzureExporter.Monitor
  ( gauges
  ) where

import qualified Azure.Monitor.Data.ListMetricValuesResponse as R
import qualified Azure.Monitor.Data.LocalizableString as LS
import qualified Azure.Monitor.Data.Metric as M
import qualified Azure.Monitor.Data.MetricValue as V
import qualified Azure.Monitor.Data.TimeSeriesElement as E
import qualified AzureExporter.Data.Gauge as G
import           Control.Lens ((^.))
import           Data.Scientific (Scientific)
import           Data.Maybe (catMaybes)
import           Data.Text (Text, pack, unpack)
import qualified Text.Casing as C

gauges :: R.ListMetricValuesResponse -> [G.Gauge]
gauges r = concatMap fGauges metrics
  where fGauges = gaugesFromMetric $ r ^. R.resourceregion
        metrics = r ^. R.value

gaugesFromMetric :: Text -> M.Metric -> [G.Gauge]
gaugesFromMetric region m = concatMap fGauges values
  where fName   = fqName (quietSnake $ m ^. M.name ^. LS.value) (m ^. M.unit)
        fGauges = gaugesFromMetricValue region fName
        values  = concatMap (^. E._data) $ m ^. M.timeseries

gaugesFromMetricValue :: Text -> (Text -> Text) -> V.MetricValue -> [G.Gauge]
gaugesFromMetricValue region fName v =
  catMaybes [ fGauge "average" $ v ^. V.average
            , fGauge "count"   $ v ^. V.count
            , fGauge "maximum" $ v ^. V.maximum
            , fGauge "minimum" $ v ^. V.minimum
            , fGauge "total"   $ v ^. V.total
            ]
    where fGauge = (\n -> gaugeFromAggregation $ fName n)

gaugeFromAggregation :: Text -> Maybe Scientific -> Maybe G.Gauge
gaugeFromAggregation _ Nothing     = Nothing
gaugeFromAggregation name (Just n) =
  Just $ G.Gauge { G._name   = name
                 , G._help   = name
                 , G._labels = []
                 , G._value  = n
                 }

-- Utilities
fqName :: Text -> Text -> Text -> Text
fqName metricName unit aggregationName =
  metricName <> "_" <> unit <> "_" <> aggregationName

-- Casing utilities
wrapText :: (String -> String) -> Text -> Text
wrapText f t = pack $ f $ unpack t

quietSnake :: Text -> Text
quietSnake = wrapText C.quietSnake
