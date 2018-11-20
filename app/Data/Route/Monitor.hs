{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Monitor
  -- * Routes
  ( metrics
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy (Text, intercalate, pack)
import Data.Time.Clock (getCurrentTime)

import Control.Lens ((^.))
import Web.Scotty.Trans (param, text)

import Auth (getTokenOrRaise, refreshTokenIfExpired)
import Control.Monad.AppEnvSTM (AppEnvSTM, liftSTM)
import Control.Monad.Either (raiseLeft)
import Data.Monitor (gauges)
import qualified Data.Monitor.ListMetricValuesRequest as M
import HTTP (request)
import Text.Gauge (renderGauge)
import Text.Timespan (timespanFrom)
import Types (AppAction)

-- | Route for Azure Monitor metrics.
metrics :: AppAction ()
metrics = do
  target      <- param "target"
  metricNames <- param "metricNames"
  aggregation <- param "aggregation"
  now         <- liftIO getCurrentTime
  token       <- refreshTokenIfExpired >> getTokenOrRaise

  let params = M.Params { M._aggregation = aggregation
                        , M._metricNames = metricNames
                        , M._resourceId  = target
                        , M._timespan    = pack $ timespanFrom now 150 90
                        }
  metrics <- raiseLeft =<< liftSTM (request $ M.request token params)
  text $ intercalate "\n" $ map renderGauge $ gauges metrics
