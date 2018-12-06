module App.Action.Gauge
    (
      -- * Stdout
      dumpGauges
      -- * Web
    , respGauges
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as LBS

import Web.Scotty.Trans (ActionT, raw)

import Data.Csv.IncrementMod (encodeNamedRecords)
import Data.Prometheus.Gauge (Gauge, renderGauges)

-- | Dump gauges in CSV format.
dumpGauges :: MonadIO m => [Gauge] -> m ()
dumpGauges = liftIO . LBS.putStr . encodeNamedRecords

-- | Response with gauges in exposition format.
respGauges :: Monad m => [Gauge] -> ActionT e m ()
respGauges = raw . toLazyByteString . renderGauges
