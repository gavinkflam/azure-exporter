module Main where

import AzureExporterExe.App (app)
import AzureExporterExe.Control.Monad.AppEnvReader
import AzureExporterExe.Data.AppEnv (AppEnv (..))
import AzureExporterExe.Data.Config (getConfig)
import Control.Concurrent.STM (newTVarIO)
import Web.Scotty.Trans (scottyT)

main :: IO ()
main = do
  appEnv <- newTVarIO =<< initialAppEnv
  scottyT 3000 (runReaderIntoIO appEnv) app

-- AppEnv
initialAppEnv :: IO AppEnv
initialAppEnv = do
  config <- getConfig
  return AppEnv { _config      = config
                , _accessToken = Nothing
                }
