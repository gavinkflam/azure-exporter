module App.Action.Gauge
    (
      -- * Stdout
      dumpGauges
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Lazy as LBS

import Data.Csv.IncrementMod (encodeNamedRecords)
import Data.Prometheus.Gauge (Gauge)

-- | Dump gauges in CSV format.
dumpGauges :: MonadIO m => [Gauge] -> m ()
dumpGauges = liftIO . LBS.putStr . encodeNamedRecords
