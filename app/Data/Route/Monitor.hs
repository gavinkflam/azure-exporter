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

import Control.Lens ((^.))
import Web.Scotty.Trans (param, raw)

import Auth (getTokenOrRaise, refreshTokenIfExpired)
import Control.Monad.AppEnvSTM (liftSTM, readAppEnv)
import Control.Monad.Either (raiseLeft)
import Control.Monad.Network.MonadHttp (httpJson)
import qualified Data.App.AppEnv as E
import qualified Data.Monitor.ListMetricValuesRequest as M
import qualified Data.Monitor.ListMetricValuesResponse as Lr
import Data.OAuth2.AcquireAccessTokenRequest as AT
import Data.Prometheus.Gauge (renderGauges)
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
        req    = M.request token params
    manager  <- liftSTM $ (^. E.httpManager) <$> readAppEnv
    metrics' <- raiseLeft =<< liftIO (httpJson AT.errorExtractor manager req)
    raw $ toLazyByteString $ renderGauges $ Lr.toGauges metrics'
