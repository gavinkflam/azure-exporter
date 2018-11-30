{-# LANGUAGE OverloadedStrings #-}

-- | Test gauges deriving from `ListMetricValuesResponse`.
module Data.Monitor.ListMetricValuesResponseSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.Monitor.ListMetricValuesResponse as Lr
import qualified Data.Monitor.LocalizableString as Ls
import qualified Data.Monitor.Metric as M
import qualified Data.Monitor.MetricValue as Mv
import qualified Data.Monitor.TimeSeriesElement as Ts
import qualified Data.Prometheus.Gauge as G
import Data.Prometheus.ToGauge (toGauges)
import Test.Hspec

-- | Spec for `ListMetricValuesResponse`.
spec :: Spec
spec =
    describe "toGauges" $
        it "derive the expected gauges from response" $
            toGauges testResponse `shouldBe` expectedGauges

-- | Text response for `toGauges` test.
testResponse :: Lr.ListMetricValuesResponse
testResponse = Lr.ListMetricValuesResponse
    { Lr._cost                       = 0
    , Lr._interval                   = testTexts ! "interval"
    , Lr._namespace                  = testTexts ! "namespace"
    , Lr._resourceregion             = testTexts ! "resourceRegion"
    , Lr._timespan                   = testTexts ! "timespan"
    , Lr._value                      =
        [ M.Metric
            { M.__id                 = testTexts ! "metricId"
            , M.__type               = testTexts ! "metricType"
            , M._name                = Ls.LocalizableString
                { Ls._value          = testTexts ! "metricName"
                , Ls._localizedValue = testTexts ! "metricName"
                }
            , M._unit                = testTexts ! "metricUnit"
            , M._timeseries          = testTimeseries
            }
        ]
    }

-- | Test timeseries for `testResponse`.
testTimeseries :: [Ts.TimeSeriesElement]
testTimeseries =
    [ Ts.TimeSeriesElement
        { Ts.__data              =
            [ Mv.MetricValue
                { Mv._average   = Just 4.2
                , Mv._count     = Nothing
                , Mv._maximum   = Nothing
                , Mv._minimum   = Nothing
                , Mv._timeStamp = testTexts ! "timestamp"
                , Mv._total     = Nothing
                }
            ]
        }
    ]

-- | Expected gauges derived from `testResponse`.
expectedGauges :: [G.Gauge]
expectedGauges =
    [ G.Gauge
        { G._name   = (testTexts ! "gaugeNamePrefix") <> "_average"
        , G._help   = (testTexts ! "gaugeNamePrefix") <> "_average"
        , G._labels =
            [ ("resource_group",    T.toLower (testTexts ! "resourceGroup"))
            , ("resource_id",       T.toLower (testTexts ! "resourceId"))
            , ("resource_name",     T.toLower (testTexts ! "resourceName"))
            , ("resource_provider", T.toLower (testTexts ! "resourceProvider"))
            , ("resource_region",   T.toLower (testTexts ! "resourceRegion"))
            , ("resource_type",     T.toLower (testTexts ! "resourceType"))
            , ("subscription_id",   T.toLower (testTexts ! "subscriptionId"))
            ]
        , G._value  = 4.2
        , G._time   = Nothing
        }
    ]

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("gaugeNamePrefix",  "azure_virtualmachines_percentage_cpu_percentage")
    , ("interval",         "PT1M")
    , ("metricId",         resourceId <> "/" <> metricType <> "/Percentage CPU")
    , ("metricName",       "Percentage CPU")
    , ("metricType",       metricType)
    , ("metricUnit",       "Percentage")
    , ("namespace",        "Microsoft.Compute")
    , ("resourceGroup",    resourceGroup)
    , ("resourceId",       resourceId)
    , ("resourceName",     resourceName)
    , ("resourceProvider", resourceProvider)
    , ("resourceType",     resourceType)
    , ("subscriptionId",   subscriptionId)
    , ("resourceRegion",   "eastasia")
    , ("timespan",         "2018-10-08T09:01:10Z/2018-10-08T09:02:10Z")
    , ("timestamp",        "2018-06-26T08:00:00Z")
    ]
  where
    metricType       = "Microsoft.Insights/metrics"
    resourceGroup    = "someGroup"
    resourceName     = "blahblah"
    resourceProvider = "Microsoft.Compute"
    resourceType     = "virtualMachines"
    subscriptionId   = "312a4ad3-78e8-4b85-aa85-fdf7041f8155"
    resourceId       = T.concat
        [ "/subscriptions/" <> subscriptionId
        , "/resourceGroups/" <> resourceGroup
        , "/providers/" <> resourceProvider
        , "/" <> resourceType <> "/" <> resourceName
        ]
