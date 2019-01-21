{-# LANGUAGE OverloadedStrings #-}

-- | Test deriving csv and exposition text for gauges.
module Data.Prometheus.GaugeSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.Prometheus.Gauge (renderGauges)
import qualified Data.Prometheus.Gauge as G

import Test.Hspec

-- | Spec for `Gauge`.
spec :: Spec
spec =
    describe "renderGauges" $
        it "derives exposition text from gauges" $
            toLazyByteString (renderGauges simpleTestGauges)
            `shouldBe` expectedSimpleTestGaugesExpositionText

-- | Simple gauges to test rendering.
simpleTestGauges :: [G.Gauge]
simpleTestGauges =
    [ G.Gauge
        { G._name   = "rpc_duration_seconds"
        , G._help   = "RPC duration in seconds."
        , G._labels =
            [ ("quantile", "0.01")
            ]
        , G._value  = 3102
        , G._time   = Just testTime
        }
    , G.Gauge
        { G._name   = "rpc_duration_seconds"
        , G._help   = "RPC duration in seconds."
        , G._labels =
            [ ("quantile", "0.05")
            ]
        , G._value  = 3272
        , G._time   = Just testTime
        }
    , G.Gauge
        { G._name   = "rpc_duration_seconds"
        , G._help   = "RPC duration in seconds."
        , G._labels =
            [ ("quantile", "0.5")
            ]
        , G._value  = 4773
        , G._time   = Just testTime
        }
    , G.Gauge
        { G._name   = "rpc_duration_seconds"
        , G._help   = "RPC duration in seconds."
        , G._labels =
            [ ("quantile", "0.9")
            ]
        , G._value  = 9001
        , G._time   = Just testTime
        }
    , G.Gauge
        { G._name   = "rpc_duration_seconds_sum"
        , G._help   = "Sum of RPC duration in seconds."
        , G._labels = []
        , G._value  = read "1.7560473e7"
        , G._time   = Just testTime
        }
    ]

-- | Time for `simpleTestGauges`.
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1530000000

-- | Expected exposition text for `simpleTestGauges`.
expectedSimpleTestGaugesExpositionText :: LBS.ByteString
expectedSimpleTestGaugesExpositionText = LBS.intercalate "\n"
    [ "# HELP rpc_duration_seconds RPC duration in seconds."
    , "# TYPE rpc_duration_seconds gauge"
    , "rpc_duration_seconds{quantile=\"0.01\"} 3102.0 1530000000"
    , ""
    , "# HELP rpc_duration_seconds RPC duration in seconds."
    , "# TYPE rpc_duration_seconds gauge"
    , "rpc_duration_seconds{quantile=\"0.05\"} 3272.0 1530000000"
    , ""
    , "# HELP rpc_duration_seconds RPC duration in seconds."
    , "# TYPE rpc_duration_seconds gauge"
    , "rpc_duration_seconds{quantile=\"0.5\"} 4773.0 1530000000"
    , ""
    , "# HELP rpc_duration_seconds RPC duration in seconds."
    , "# TYPE rpc_duration_seconds gauge"
    , "rpc_duration_seconds{quantile=\"0.9\"} 9001.0 1530000000"
    , ""
    , "# HELP rpc_duration_seconds_sum Sum of RPC duration in seconds."
    , "# TYPE rpc_duration_seconds_sum gauge"
    , "rpc_duration_seconds_sum 17560473.0 1530000000"
    ]
