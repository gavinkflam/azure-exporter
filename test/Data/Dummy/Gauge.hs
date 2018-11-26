{-# LANGUAGE OverloadedStrings #-}

module Data.Dummy.Gauge
    (
      -- * Gauge
      gauge
    , usageGauge
    , costGauge
      -- * Text
    , gaugeText
      -- * Csv
    , gaugesCsv
      -- * ListMetricValuesResponse
    , listMetricValuesResponse
    ) where

import Data.Text (Text, intercalate, pack, toLower)

import Control.Lens ((^.))

import qualified Data.Dummy.Text as T
import qualified Data.Dummy.Time as TI
import qualified Data.Gauge as G
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
        , ("unit",              T.unit)
        ]
    , G._value  = 4.2
    , G._time   = Nothing
    }

-- | Dummy `Gauge` for usage.
usageGauge :: G.Gauge
usageGauge = G.Gauge
    { G._name   = "azure_storage_grs_data_stored_1_gb_month_usage"
    , G._help   = "azure_storage_grs_data_stored_1_gb_month_usage"
    , G._labels =
        [ ("resource_group",    toLower T.resourceGroup2)
        , ("resource_id",       toLower T.resourceId2)
        , ("resource_name",     toLower T.resourceName2)
        , ("resource_provider", toLower T.resourceProvider2)
        , ("resource_region",   toLower T.resourceRegion2)
        , ("resource_type",     toLower T.resourceType2)
        , ("subscription_id",   T.subscriptionId)
        , ("unit",              T.unit2)
        ]
    , G._value  = 22.3
    , G._time   = Just TI.timeFrom
    }

-- | Dummy `Gauge` for cost.
costGauge :: G.Gauge
costGauge = G.Gauge
    { G._name   = "azure_storage_grs_data_stored_1_gb_month_cost"
    , G._help   = "azure_storage_grs_data_stored_1_gb_month_cost"
    , G._labels =
        [ ("currency",          T.currency)
        , ("resource_group",    toLower T.resourceGroup2)
        , ("resource_id",       toLower T.resourceId2)
        , ("resource_name",     toLower T.resourceName2)
        , ("resource_provider", toLower T.resourceProvider2)
        , ("resource_region",   toLower T.resourceRegion2)
        , ("resource_type",     toLower T.resourceType2)
        , ("subscription_id",   T.subscriptionId)
        , ("unit",              T.unit2)
        , ("unit_cost",         pack $ show T.unitCost)
        ]
    , G._value  = 4.683
    , G._time   = Just TI.timeFrom
    }

-- | Prometheus exporter syntax for `gauge`.
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

-- | Csv output for `usageGauge` and `costGauge`.
gaugesCsv :: Text
gaugesCsv = intercalate "\n"
    [ "series,value,timestamp,label_currency,label_resource_group" <>
        ",label_resource_id,label_resource_name,label_resource_provider" <>
        ",label_resource_region,label_resource_type,label_subscription_id" <>
        ",label_unit,label_unit_cost"
    , (usageGauge ^. G.name) <> "," <> pack (show (usageGauge ^. G.value)) <>
        "," <> pack (show TI.unixTimestampFrom) <> "," <>
        "," <> toLower T.resourceGroup2 <> "," <> toLower T.resourceId2 <>
        "," <> toLower T.resourceName2 <> "," <> toLower T.resourceProvider2 <>
        "," <> toLower T.resourceRegion2 <> "," <> toLower T.resourceType2 <>
        "," <> T.subscriptionId <>
        "," <> T.unit2 <> ","
    , (costGauge ^. G.name) <> "," <> pack (show (costGauge ^. G.value)) <>
        "," <> pack (show TI.unixTimestampFrom) <> "," <> T.currency <>
        "," <> toLower T.resourceGroup2 <> "," <> toLower T.resourceId2 <>
        "," <> toLower T.resourceName2 <> "," <> toLower T.resourceProvider2 <>
        "," <> toLower T.resourceRegion2 <> "," <> toLower T.resourceType2 <>
        "," <> T.subscriptionId <>
        "," <> T.unit2 <> "," <> pack (show T.unitCost)
    ]

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
