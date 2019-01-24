{-# LANGUAGE OverloadedStrings #-}

-- | Test gauges deriving for `ListMetricValuesResponse`.
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
import Test.Hspec

-- | Spec for `ListMetricValuesResponse`.
spec :: Spec
spec =
    describe "toGauges" $
        it "derives the expected gauges from response" $
            Lr.toGauges testResponse `shouldBe` expectedGauges

-- | Text response for `toGauges` test.
testResponse :: Lr.ListMetricValuesResponse
testResponse = Lr.ListMetricValuesResponse
    { Lr.cost                       = 0
    , Lr.interval                   = testTexts ! "interval"
    , Lr.namespace                  = testTexts ! "namespace"
    , Lr.resourceregion             = testTexts ! "resourceRegion"
    , Lr.timespan                   = testTexts ! "timespan"
    , Lr.value                      =
        [ M.Metric
            { M._id                 = testTexts ! "metricId"
            , M._type               = testTexts ! "metricType"
            , M.name                = Ls.LocalizableString
                { Ls.value          = testTexts ! "metricName"
                , Ls.localizedValue = testTexts ! "metricName"
                }
            , M.unit                = testTexts ! "metricUnit"
            , M.timeseries          = testTimeseries
            }
        ]
    }

-- | Test timeseries for `testResponse`.
testTimeseries :: [Ts.TimeSeriesElement]
testTimeseries =
    [ Ts.TimeSeriesElement
        { Ts._data              =
            [ Mv.MetricValue
                { Mv.average   = Just 4.2
                , Mv.count     = Nothing
                , Mv.maximum   = Nothing
                , Mv.minimum   = Nothing
                , Mv.timeStamp = testTexts ! "timestamp"
                , Mv.total     = Nothing
                }
            ]
        }
    ]

-- | Expected gauges derived from `testResponse`.
expectedGauges :: [G.Gauge]
expectedGauges =
    [ G.Gauge
        { G.name   = (testTexts ! "gaugeNamePrefix") <> "_average"
        , G.help   = (testTexts ! "gaugeNamePrefix") <> "_average"
        , G.labels =
            [ ("resource_group",    T.toLower (testTexts ! "resourceGroup"))
            , ("resource_id",       T.toLower (testTexts ! "resourceId"))
            , ("resource_name",     T.toLower (testTexts ! "resourceName"))
            , ("resource_provider", T.toLower (testTexts ! "resourceProvider"))
            , ("resource_region",   T.toLower (testTexts ! "resourceRegion"))
            , ("resource_type",     T.toLower (testTexts ! "resourceType"))
            , ("subscription_id",   T.toLower (testTexts ! "subscriptionId"))
            ]
        , G.value  = 4.2
        , G.time   = Nothing
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
