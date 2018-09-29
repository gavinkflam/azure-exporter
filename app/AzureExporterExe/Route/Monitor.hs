{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Route.Monitor
  ( metrics
  ) where

import qualified Azure.Request.Monitor.ListMetricValues as M
import           Azure.Text.Timespan (getLastMinuteTimespan)
import           AzureExporter.Monitor (gauges)
import           AzureExporter.Text.Gauge (renderGauge)
import qualified AzureExporterExe.Auth as A
import           AzureExporterExe.Control.Monad.AppEnvSTM
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import qualified AzureExporterExe.Data.AccessToken as AT
import qualified AzureExporterExe.Data.AppEnv as E
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Web.Scotty.Trans

metrics :: ActionT Text AppEnvSTM ()
metrics = do
  resourceId  <- param "resourceId"
  metricNames <- param "metricNames"
  aggregation <- param "aggregation"
  timespan    <- liftIO getLastMinuteTimespan
  token       <- A.resolveToken

  let params = M.Params { M._aggregation = aggregation
                        , M._metricNames = metricNames
                        , M._resourceId  = resourceId
                        , M._timespan    = pack timespan
                        }
  metrics <- raiseLeft =<< (liftIO $ M.listMetricValues token params)
  text $ intercalate "\n\n" $ map renderGauge $ gauges metrics