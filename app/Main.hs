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
      token      <- eitherRaise $ liftIO $ acquireAccessToken config

      let params = M.Params { M._aggregation = "average"
                            , M._metricNames = "Percentage CPU"
                            , M._resourceId  = resourceId
                            , M._timespan    = dummyTimespan
                            }
      metrics <- eitherRaise $ liftIO $ M.listMetricValues token params
      text $ L.pack $ show metrics

-- Dummy data
dummyTimespan :: Text
dummyTimespan = "2018-09-26T04:03:30.843Z/2018-09-26T04:04:30.843Z"

-- Utilities
eitherRaise :: Either String a -> ActionM a
eitherRaise (Left m)  = raise $ L.pack m
eitherRaise (Right x) = return x

-- AcquireAccessToken
acquireAccessToken :: Config -> IO (Either String Text)
acquireAccessToken c = do
  let params = T.Params { T._clientId     = c ^. clientId
                        , T._clientSecret = c ^. clientSecret
                        , T._tenantId     = c ^. tenantId
                        }
  res <- T.acquireAccessToken params
  return $ fmap (^. TR.accessToken) res
