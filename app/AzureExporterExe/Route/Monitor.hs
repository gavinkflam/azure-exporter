{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Route.Monitor
  ( metrics
  ) where

import qualified Azure.Request.Monitor.ListMetricValues as M
import           Azure.Text.Timespan (timespanFrom)
import           AzureExporter.Monitor (gauges)
import           AzureExporterExe.HTTP (request)
import           AzureExporter.Text.Gauge (renderGauge)
import           AzureExporterExe.Auth (getTokenOrRaise, refreshTokenIfExpired)
import           AzureExporterExe.Control.Monad.AppEnvSTM (AppEnvSTM, liftSTM)
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Data.Time.Clock (getCurrentTime)
import           Web.Scotty.Trans (ActionT, param, text)

metrics :: ActionT Text AppEnvSTM ()
metrics = do
  target      <- param "target"
  metricNames <- param "metricNames"
  aggregation <- param "aggregation"
  now         <- liftIO getCurrentTime
  token       <- refreshTokenIfExpired >> getTokenOrRaise

  let params = M.Params { M._aggregation = aggregation
                        , M._metricNames = metricNames
                        , M._resourceId  = target
                        , M._timespan    = pack $ timespanFrom now 60 0
                        }
  metrics <- raiseLeft =<< liftSTM (request $ M.request token params)
  text $ intercalate "\n" $ map renderGauge $ gauges metrics
