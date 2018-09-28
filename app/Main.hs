{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Azure.Monitor.ListMetricValues as M
import qualified Azure.OAuth2.AcquireAccessToken as T
import qualified Azure.OAuth2.Data.AcquireAccessTokenResponse as TR
import           AzureExporter.Monitor (gauges)
import           AzureExporter.Text.Gauge (renderGauge)
import qualified AzureExporterExe.Control.AppState as AS
import           AzureExporterExe.Control.Scotty (liftE)
import qualified AzureExporterExe.Data.AccessToken as AT
import qualified AzureExporterExe.Data.Config as C
import           AzureExporterExe.Data.Timespan (getLastMinuteTimespan)
import           Control.Concurrent.STM (newTVarIO)
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader (runReaderT)
import           Data.Text.Lazy (Text, intercalate, pack)
import           System.Exit (die)
import           Web.Scotty.Trans

main :: IO ()
main = do
  state <- initialAppState
  sync  <- newTVarIO state
  let runActionToIO m = runReaderT (AS.runWebM m) sync
  scottyT 3000 runActionToIO app

app :: ScottyT Text AS.WebM ()
app = do
  get "/monitor/metrics" $ do
    resourceId  <- param "resourceId"
    metricNames <- param "metricNames"
    aggregation <- param "aggregation"
    timespan    <- liftIO getLastMinuteTimespan
    token       <- AS.webM $ AS.get $ (^. AT.accessToken) . (^. AS.accessToken)

    let params = M.Params { M._aggregation = aggregation
                          , M._metricNames = metricNames
                          , M._resourceId  = resourceId
                          , M._timespan    = pack timespan
                          }
    metrics <- liftE $ liftIO $ M.listMetricValues token params
    text $ intercalate "\n\n" $ map renderGauge $ gauges metrics

-- AppState
initialAppState :: IO AS.AppState
initialAppState = do
  config <- C.getConfig
  token <- (T.acquireAccessToken $ acquireTokenParams config) >>= dieIfError
  return AS.AppState { AS._config      = config
                     , AS._accessToken = AT.fromResponse token
                     }

acquireTokenParams :: C.Config -> T.Params
acquireTokenParams c =
  T.Params { T._clientId     = c ^. C.clientId
           , T._clientSecret = c ^. C.clientSecret
           , T._tenantId     = c ^. C.tenantId
           }

-- IO Utilities
dieIfError :: Either String a -> IO a
dieIfError (Left m)  = die m
dieIfError (Right x) = return x
