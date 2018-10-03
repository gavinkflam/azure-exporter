{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Route.Monitor
  ( metrics
  ) where

import qualified Azure.Request.Monitor.ListMetricValues as M
import           Azure.Text.Timespan (getTimespanFromNow)
import           AzureExporter.Monitor (gauges)
import           AzureExporter.Text.Gauge (renderGauge)
import           AzureExporterExe.Auth (getTokenOrRaise, refreshTokenIfExpired)
import           AzureExporterExe.Control.Monad.AppEnvSTM (AppEnvSTM)
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Web.Scotty.Trans (ActionT, param, text)

metrics :: ActionT Text AppEnvSTM ()
metrics = do
  target      <- param "target"
  metricNames <- param "metricNames"
  aggregation <- param "aggregation"
  timespan    <- liftIO $ getTimespanFromNow 60 0
  token       <- refreshTokenIfExpired >> getTokenOrRaise

  let params = M.Params { M._aggregation = aggregation
                        , M._metricNames = metricNames
                        , M._resourceId  = target
                        , M._timespan    = pack timespan
                        }
  metrics <- raiseLeft =<< liftIO (M.listMetricValues token params)
  text $ intercalate "\n\n" $ map renderGauge $ gauges metrics
