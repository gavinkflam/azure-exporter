module App.Action.AppEnv
    (
      -- * Construction
      constructAppEnv
    ) where

import Control.Concurrent.STM.TVar (newTVarIO)

import App.Http.Auth (refreshToken)
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- | Construct `AppEnv` from environment variables and making HTTP request.
constructAppEnv :: IO En.AppEnv
constructAppEnv = do
    config  <- Cf.getConfig
    manager <- newManager tlsManagerSettings
    var     <- newTVarIO undefined

    refreshToken manager config var

    return En.AppEnv
        { En.accessToken = var
        , En.config      = config
        , En.httpManager = manager
        }
