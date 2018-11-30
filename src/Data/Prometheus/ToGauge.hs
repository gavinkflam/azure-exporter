module Data.Prometheus.ToGauge
    (
      -- * Types
      ToGauge (..)
    ) where

import Data.Prometheus.Gauge (Gauge)

-- | Typeclass to derive `Gauge`.
class ToGauge a where
    toGauges :: a -> [Gauge]
