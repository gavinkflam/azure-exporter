{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified AzureExporter.Monitor.ListMetricValues as M
import qualified AzureExporter.Token as T
import           AzureExporterExe.Config
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Text (Text)
import qualified Data.Text.Lazy as L
import           Web.Scotty

main :: IO ()
main = do
  config <- getConfig

  scotty 3000 $ do
    get "/monitor/metrics" $ do
      resourceId <- param "resourceId"
      token      <- liftIO $ acquireToken config
      metrics    <-
        liftIO $ M.listMetricValues token $ listMetricValuesParams resourceId
      case metrics of
        Nothing -> raise "No metrics found"
        Just m  -> text $ L.pack $ show m

listMetricValuesParams :: Text -> M.ListMetricValuesParams
listMetricValuesParams resourceId =
  M.ListMetricValuesParams { M._aggregation = "average"
                           , M._metricNames = "Percentage CPU"
                           , M._resourceId  = resourceId
                           , M._timespan    = "2018-09-26T04:03:30.843Z/2018-09-26T04:04:30.843Z"
                           }

acquireTokenParams :: Config -> T.AcquireTokenParams
acquireTokenParams c =
  T.AcquireTokenParams { T._clientId     = c ^. clientId
                       , T._clientSecret = c ^. clientSecret
                       , T._tenantId     = c ^. tenantId
                       }

acquireToken :: Config -> IO Text
acquireToken c = do
  res <- T.acquireToken $ acquireTokenParams c
  case res of
    Just r  -> return $ r ^. T.accessToken
    Nothing -> return $ "" -- TODO: properly handle errors
