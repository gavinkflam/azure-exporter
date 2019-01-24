module Server
    (
      -- * Server
      runServer
    ) where

import Control.Monad.Reader (runReaderT)

import Web.Scotty.Trans (scottyT)

import App (app)
import Data.App.AppEnv (AppEnv)
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf

-- | Start the exporter HTTP server.
runServer :: AppEnv -> IO ()
runServer appEnv =
    scottyT (Cf.port $ En.config appEnv) (`runReaderT` appEnv) app
