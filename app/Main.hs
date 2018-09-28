{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Azure.Data.OAuth2.AcquireAccessTokenResponse as TR
import qualified Azure.Request.Monitor.ListMetricValues as M
import qualified Azure.Request.OAuth2.AcquireAccessToken as T
import           Azure.Text.Timespan (getLastMinuteTimespan)
import           AzureExporter.Monitor (gauges)
import           AzureExporter.Text.Gauge (renderGauge)
import           AzureExporterExe.Control.Monad.AppEnvReader
import           AzureExporterExe.Control.Monad.Either (raiseLeft, dieLeft)
import qualified AzureExporterExe.Data.AccessToken as AT
import qualified AzureExporterExe.Data.AppEnv as E
import qualified AzureExporterExe.Data.Config as C
import           Control.Concurrent.STM (newTVarIO)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text, intercalate, pack)
import           Web.Scotty.Trans

main :: IO ()
main = do
  state <- initialAppState >>= newTVarIO
  scottyT 3000 (runReaderIntoIO state) app

app :: ScottyT Text AppEnvReader ()
app = do
  get "/monitor/metrics" $ do
    resourceId  <- param "resourceId"
    metricNames <- param "metricNames"
    aggregation <- param "aggregation"
    timespan    <- liftIO getLastMinuteTimespan
    token       <- liftR $ getR $ (^. AT.accessToken) . (^. E.accessToken)

    let params = M.Params { M._aggregation = aggregation
                          , M._metricNames = metricNames
                          , M._resourceId  = resourceId
                          , M._timespan    = pack timespan
                          }
    metrics <- raiseLeft =<< (liftIO $ M.listMetricValues token params)
    text $ intercalate "\n\n" $ map renderGauge $ gauges metrics

-- AppEnv
initialAppState :: IO E.AppEnv
initialAppState = do
  config <- C.getConfig
  token <- dieLeft =<< (T.acquireAccessToken $ acquireTokenParams config)
  return E.AppEnv { E._config      = config
                  , E._accessToken = AT.fromResponse token
                  }

acquireTokenParams :: C.Config -> T.Params
acquireTokenParams c =
  T.Params { T._clientId     = c ^. C.clientId
           , T._clientSecret = c ^. C.clientSecret
           , T._tenantId     = c ^. C.tenantId
           }
