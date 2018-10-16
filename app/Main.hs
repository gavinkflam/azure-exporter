module Main where

import AzureExporterExe.App (app)
import AzureExporterExe.Control.Monad.AppEnvSTM
import AzureExporterExe.Data.AppEnv (AppEnv (..))
import AzureExporterExe.Data.Config (getConfig, port)
import Control.Concurrent.STM (newTVarIO)
import Control.Lens ((^.))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Scotty.Trans (scottyT)

-- | Entry point for exporter HTTP server.
main :: IO ()
main = do
  config  <- getConfig
  manager <- newManager tlsManagerSettings
  appEnv  <- newTVarIO AppEnv { _accessToken = Nothing
                              , _config      = config
                              , _httpManager = manager
                              }
  scottyT (config ^. port) (runAppEnvSTMIntoIO appEnv) app
