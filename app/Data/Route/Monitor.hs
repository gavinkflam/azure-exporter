{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Monitor
    (
      -- * Routes
      metrics
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder (toLazyByteString)
import Data.Text (pack)
import Data.Time.Clock (getCurrentTime)

import Web.Scotty.Trans (param, raw)

import Auth (getTokenOrRaise, refreshTokenIfExpired)
import Control.Monad.AppEnvSTM (liftSTM)
import Control.Monad.Either (raiseLeft)
import qualified Data.Monitor.ListMetricValuesRequest as M
import Data.Prometheus.Gauge (renderGauges)
import Data.Prometheus.ToGauge (multiToGauges)
import HTTP (request)
import Text.Monitor.Timespan (timespanFrom)
import Types (AppAction)

-- | Route for Azure Monitor metrics.
metrics :: AppAction ()
metrics = do
    target      <- param "target"
    metricNames <- param "metricNames"
    aggregation <- param "aggregation"
    now         <- liftIO getCurrentTime
    token       <- refreshTokenIfExpired >> getTokenOrRaise

    let params = M.Params
          { M._aggregation = aggregation
          , M._metricNames = metricNames
          , M._resourceId  = target
          , M._timespan    = pack $ timespanFrom now 150 90
          }
    metrics' <- raiseLeft =<< liftSTM (request $ M.request token params)
    raw $ toLazyByteString $ renderGauges $ multiToGauges metrics'
