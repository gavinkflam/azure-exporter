{-# LANGUAGE OverloadedStrings #-}

module Data.Dummy.Gauge
    (
      -- * Gauge
      gauge
    , gaugeText
      -- * ListMetricValuesResponse
    , listMetricValuesResponse
    ) where

import Data.Text.Lazy (Text, intercalate, pack, toLower)

import Control.Lens ((^.))

import qualified Data.Gauge as G
import qualified Data.Dummy.Text as T
import qualified Data.Dummy.Time as TI
import qualified Data.Monitor.ListMetricValuesResponse as R
import qualified Data.Monitor.LocalizableString as LS
import qualified Data.Monitor.Metric as M
import qualified Data.Monitor.MetricValue as V
import qualified Data.Monitor.TimeSeriesElement as E

-- | Dummy `Gauge`.
gauge :: G.Gauge
gauge = G.Gauge
    { G._name   = "azure_virtualmachines_percentage_cpu_percentage_average"
    , G._help   = "azure_virtualmachines_percentage_cpu_percentage_average"
    , G._labels =
        [ ("resource_group",    toLower T.resourceGroup)
        , ("resource_id",       toLower T.resourceId)
        , ("resource_name",     toLower T.resourceName)
        , ("resource_provider", toLower T.resourceProvider)
        , ("resource_region",   toLower T.resourceRegion)
        , ("resource_type",     toLower T.resourceType)
        , ("subscription_id",   T.subscriptionId)
        ]
    , G._value  = 4.2
    , G._time   = Nothing
    }

-- | `gauge` in Prometheus exporter syntax.
gaugeText :: Text
gaugeText = intercalate "\n"
    [ "# HELP " <> (gauge ^. G.name) <> " " <> (gauge ^. G.help)
    , "# TYPE " <> (gauge ^. G.name) <> " gauge"
    , (gauge ^. G.name) <> "{" <> labels <> "} " <> value
    ]
  where
    label (k, v) = k <> "=\"" <> v <> "\""
    labels = intercalate "," $ map label (gauge ^. G.labels)
    value  = pack $ show (gauge ^. G.value)

-- | The type for metrics.
metricsType :: Text
metricsType = "Microsoft.Insights/metrics"

-- | The corresponding `ListMetricValuesResponse` for `gauge`.
listMetricValuesResponse :: R.ListMetricValuesResponse
listMetricValuesResponse = R.ListMetricValuesResponse
    { R._cost                        = 0
    , R._interval                    = "PT1M"
    , R._namespace                   = "Microsoft.Compute/virtualMachines"
    , R._resourceregion              = T.resourceRegion
    , R._timespan                    = pack TI.dummyTimespan
    , R._value                       =
        [ M.Metric
            { M.__id                 =
                T.resourceId <> "/" <> metricsType <> "/Percentage CPU"
            , M.__type               = metricsType
            , M._name                = LS.LocalizableString
                { LS._value          = "Percentage CPU"
                , LS._localizedValue = "Percentage CPU"
                }
            , M._unit                = "Percentage"
            , M._timeseries          = listMetricValuesTimeseries
            }
        ]
    }

listMetricValuesTimeseries :: [E.TimeSeriesElement]
listMetricValuesTimeseries =
    [ E.TimeSeriesElement
        { E.__data             =
            [ V.MetricValue
                { V._average   = Just 4.2
                , V._count     = Nothing
                , V._maximum   = Nothing
                , V._minimum   = Nothing
                , V._timeStamp = TI.timestampFrom
                , V._total     = Nothing
                }
            ]
        }
    ]
