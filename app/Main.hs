{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Azure.Monitor.ListMetricValues as M
import qualified Azure.OAuth2.AcquireAccessToken as T
import qualified Azure.OAuth2.Data.AcquireAccessTokenResponse as TR
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
      token      <- liftIO $ acquireAccessToken config
      metrics    <-
        liftIO $ M.listMetricValues token $ listMetricValuesParams resourceId
      case metrics of
        Nothing -> raise "No metrics found"
        Just m  -> text $ L.pack $ show m

listMetricValuesParams :: Text -> M.Params
listMetricValuesParams resourceId =
  M.Params { M._aggregation = "average"
           , M._metricNames = "Percentage CPU"
           , M._resourceId  = resourceId
           , M._timespan    = "2018-09-26T04:03:30.843Z/2018-09-26T04:04:30.843Z"
           }

acquireTokenParams :: Config -> T.Params
acquireTokenParams c =
  T.Params { T._clientId     = c ^. clientId
           , T._clientSecret = c ^. clientSecret
           , T._tenantId     = c ^. tenantId
           }

acquireAccessToken :: Config -> IO Text
acquireAccessToken c = do
  res <- T.acquireAccessToken $ acquireTokenParams c
  case res of
    Just r  -> return $ r ^. TR.accessToken
    Nothing -> return $ "" -- TODO: properly handle errors
