{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Azure.Request.OAuth2.AcquireAccessToken as T
import           AzureExporterExe.App (app)
import           AzureExporterExe.Control.Monad.AppEnvReader
import           AzureExporterExe.Control.Monad.Either (dieLeft)
import qualified AzureExporterExe.Data.AccessToken as AT
import qualified AzureExporterExe.Data.AppEnv as E
import qualified AzureExporterExe.Data.Config as C
import           Control.Concurrent.STM (newTVarIO)
import           Control.Lens ((^.))
import           Web.Scotty.Trans (scottyT)

main :: IO ()
main = do
  state <- newTVarIO =<< initialAppState
  scottyT 3000 (runReaderIntoIO state) app

-- AppEnv
initialAppState :: IO E.AppEnv
initialAppState = do
  config <- C.getConfig
  token  <- dieLeft =<< (T.acquireAccessToken $ acquireTokenParams config)
  return E.AppEnv { E._config      = config
                  , E._accessToken = AT.fromResponse token
                  }

acquireTokenParams :: C.Config -> T.Params
acquireTokenParams c =
  T.Params { T._clientId     = c ^. C.clientId
           , T._clientSecret = c ^. C.clientSecret
           , T._tenantId     = c ^. C.tenantId
           }
