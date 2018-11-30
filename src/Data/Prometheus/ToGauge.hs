module Data.Prometheus.ToGauge
    (
      -- * Types
      ToGauge (..)
      -- * Gauge
    , multiToGauges
    ) where

import Data.Prometheus.Gauge (Gauge)

-- | Typeclass to derive `Gauge`.
class ToGauge a where
    toGauges :: a -> [Gauge]

-- | Derive list of `Gauge` from list of `ToGauge`.
multiToGauges :: ToGauge a => [a] -> [Gauge]
multiToGauges = concatMap toGauges
