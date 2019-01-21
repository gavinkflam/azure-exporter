module App.Action.Gauge
    (
      -- * Web
      respGauges
    ) where

import Data.ByteString.Builder (toLazyByteString)

import Web.Scotty.Trans (ActionT, raw)

import Data.Prometheus.Gauge (Gauge, renderGauges)

-- | Response with gauges in exposition format.
respGauges :: Monad m => [Gauge] -> ActionT e m ()
respGauges = raw . toLazyByteString . renderGauges
