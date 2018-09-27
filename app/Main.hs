{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Azure.Monitor.ListMetricValues as M
import qualified Azure.OAuth2.AcquireAccessToken as T
import qualified Azure.OAuth2.Data.AcquireAccessTokenResponse as TR
import           AzureExporter.Monitor (gauges)
import           AzureExporter.Text.Gauge (renderGauge)
import           AzureExporterExe.Config
import           AzureExporterExe.Control.Scotty (liftE)
import           AzureExporterExe.Data.Timespan (getLastMinuteTimespan)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Web.Scotty

main :: IO ()
main = do
  config <- getConfig

  scotty 3000 $ do
    get "/monitor/metrics" $ do
      resourceId  <- param "resourceId"
      metricNames <- param "metricNames"
      aggregation <- param "aggregation"
      timespan    <- liftIO getLastMinuteTimespan
      token       <- liftE $ liftIO $ acquireAccessToken config

      let params = M.Params { M._aggregation = aggregation
                            , M._metricNames = metricNames
                            , M._resourceId  = resourceId
                            , M._timespan    = pack timespan
                            }
      metrics <- liftE $ liftIO $ M.listMetricValues token params
      text $ intercalate "\n\n" $ map renderGauge $ gauges metrics

-- AcquireAccessToken
acquireAccessToken :: Config -> IO (Either String Text)
acquireAccessToken c = do
  let params = T.Params { T._clientId     = c ^. clientId
                        , T._clientSecret = c ^. clientSecret
                        , T._tenantId     = c ^. tenantId
                        }
  res <- T.acquireAccessToken params
  return $ fmap (^. TR.accessToken) res
