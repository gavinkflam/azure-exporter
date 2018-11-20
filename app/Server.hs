module Server
  -- * Server
  ( runServer
  ) where

import Control.Concurrent.STM (newTVarIO)

import Control.Lens ((^.))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Scotty.Trans (scottyT)

import App (app)
import Control.Monad.AppEnvSTM
import Data.AppEnv (AppEnv (..))
import Data.Config (getConfig, port)

-- | Start the exporter HTTP server.
runServer :: IO ()
runServer = do
  config  <- getConfig
  manager <- newManager tlsManagerSettings
  appEnv  <- newTVarIO AppEnv { _accessToken = Nothing
                              , _config      = config
                              , _httpManager = manager
                              }
  scottyT (config ^. port) (runAppEnvSTMIntoIO appEnv) app