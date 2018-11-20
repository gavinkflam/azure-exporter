{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Route.Monitor
  -- * Routes
  ( metrics
  ) where

import qualified Azure.Request.Monitor.ListMetricValues as M
import           Text.Timespan (timespanFrom)
import           AzureExporter.Monitor (gauges)
import           Text.Gauge (renderGauge)
import           AzureExporterExe.HTTP (request)
import           AzureExporterExe.Auth (getTokenOrRaise, refreshTokenIfExpired)
import           AzureExporterExe.Control.Monad.AppEnvSTM (AppEnvSTM, liftSTM)
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import           AzureExporterExe.Types (AppAction)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Data.Time.Clock (getCurrentTime)
import           Web.Scotty.Trans (param, text)

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
