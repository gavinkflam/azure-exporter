{-# LANGUAGE OverloadedStrings #-}

module Data.Prometheus.TestData
    (
      -- * Test data
      testGauges
      -- * Result
    , expectedHeader
    , expectedCsv
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.List (sort, (\\))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)

import Control.Lens ((^.))
import Data.Csv (Header)
import Data.HashMap.Strict (HashMap, keys, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.Prometheus.Gauge as G
import qualified Data.Vector as V
import Text.Scientific (showFixed)

-- | Dummy `Gauge` list.
testGauges :: [G.Gauge]
testGauges = [usageGauge, costGauge]

-- | Dummy `Gauge` for usage.
usageGauge :: G.Gauge
usageGauge = G.Gauge
    { G._name   = "azure_storage_grs_data_stored_1_gb_month_usage"
    , G._help   = "azure_storage_grs_data_stored_1_gb_month_usage"
    , G._labels =
        [ ("resource_group",    testLabels ! "resource_group")
        , ("resource_id",       testLabels ! "resource_id")
        , ("resource_name",     testLabels ! "resource_name")
        , ("resource_provider", testLabels ! "resource_provider")
        , ("resource_region",   testLabels ! "resource_region")
        , ("resource_type",     testLabels ! "resource_type")
        , ("subscription_id",   testLabels ! "subscription_id")
        , ("unit",              testLabels ! "unit")
        ]
    , G._value  = 22.3
    , G._time   = Just testTime
    }

-- | Dummy `Gauge` for cost.
costGauge :: G.Gauge
costGauge = G.Gauge
    { G._name   = "azure_storage_grs_data_stored_1_gb_month_cost"
    , G._help   = "azure_storage_grs_data_stored_1_gb_month_cost"
    , G._labels =
        [ ("currency",          testLabels ! "currency")
        , ("resource_group",    testLabels ! "resource_group")
        , ("resource_id",       testLabels ! "resource_id")
        , ("resource_name",     testLabels ! "resource_name")
        , ("resource_provider", testLabels ! "resource_provider")
        , ("resource_region",   testLabels ! "resource_region")
        , ("resource_type",     testLabels ! "resource_type")
        , ("subscription_id",   testLabels ! "subscription_id")
        , ("unit",              testLabels ! "unit")
        , ("unit_cost",         testLabels ! "unit_cost")
        ]
    , G._value  = 2.1185
    , G._time   = Just testTime
    }

-- | Posix seconds for `testTime`.
testTimePosixSeconds :: POSIXTime
testTimePosixSeconds = 1530000000

-- | Time for `usageGauge` and `costGauge`.
testTime :: UTCTime
testTime = posixSecondsToUTCTime testTimePosixSeconds

-- | testLabels for `usageGauge` and `costGauge`.
testLabels :: HashMap Text Text
testLabels = HM.fromList
    [ ("currency",          "USD")
    , ("resource_group",    "DummyGroup")
    , ("resource_id",       "/subscriptions/312-78e8/blah-blah-blah")
    , ("resource_name",     "dummyaccount")
    , ("resource_provider", "microsoft.storage")
    , ("resource_region",   "apeast")
    , ("resource_type",     "storageaccounts")
    , ("subscription_id",   "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    , ("unit",              "1 GB/Month")
    , ("unit_cost",         "0.095")
    ]

-- | Expected header derived from `testGauges`.
expectedHeader :: Header
expectedHeader =
    V.fromList $ coreHeader ++ map ("label_" <>) labelHeader
  where
    coreHeader  = ["series", "value", "timestamp"]
    labelHeader = sort $ map encodeUtf8 (keys testLabels) \\ coreHeader

-- | Expected csv text derived from `testGauges`.
expectedCsv :: LBS.ByteString
expectedCsv = LBS.intercalate "\r\n"
    [ LBS.intercalate "," $ map LBS.fromStrict $ V.toList expectedHeader
    , LBS.intercalate "," $ map lbs
        [ usageGauge ^. G.name
        , pack $ showFixed (usageGauge ^. G.value)
        , pack $ init $ show testTimePosixSeconds
        , ""
        , testLabels ! "resource_group"
        , testLabels ! "resource_id"
        , testLabels ! "resource_name"
        , testLabels ! "resource_provider"
        , testLabels ! "resource_region"
        , testLabels ! "resource_type"
        , testLabels ! "subscription_id"
        , testLabels ! "unit"
        , ""
        ]
    , LBS.intercalate "," $ map lbs
        [ costGauge ^. G.name
        , pack $ showFixed (costGauge ^. G.value)
        , pack $ init $ show testTimePosixSeconds
        , testLabels ! "currency"
        , testLabels ! "resource_group"
        , testLabels ! "resource_id"
        , testLabels ! "resource_name"
        , testLabels ! "resource_provider"
        , testLabels ! "resource_region"
        , testLabels ! "resource_type"
        , testLabels ! "subscription_id"
        , testLabels ! "unit"
        , testLabels ! "unit_cost"
        ]
    , ""
    ]

-- | Convert `Text` to lazy `ByteString`.
lbs :: Text -> LBS.ByteString
lbs = LBS.fromStrict . encodeUtf8
