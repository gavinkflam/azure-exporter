module Main where

import AzureExporterExe.App (app)
import AzureExporterExe.Control.Monad.AppEnvSTM
import AzureExporterExe.Data.AppEnv (AppEnv (..))
import AzureExporterExe.Data.Config (getConfig, port)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Web.Scotty.Trans (scottyT)

main :: IO ()
main = do
  config <- getConfig
  appEnv <- newTVarIO AppEnv { _config      = config
                             , _accessToken = Nothing
                             }
  scottyT (config ^. port) (runAppEnvSTMIntoIO appEnv) app
