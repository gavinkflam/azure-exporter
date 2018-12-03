module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import System.Environment (getArgs)
import System.Exit (die)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Auth (acquireToken)
import Control.Monad.Either (dieLeft)
import qualified Data.App.AccessToken as Ak
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf
import Server (runServer)
import UsageDump (dumpUsage)

-- | Entry point for Azure exporter executable.
main :: IO ()
main = do
    appEnv <- constructAppEnv
    args   <- getArgs

    case args of
        ["server"]             -> runServer
        ["dump-usage", t1, t2] -> runReaderT (dumpUsage t1 t2) appEnv
        _                      -> die $ argsError args

-- | Construct `AppEnv` from environment variables and making HTTP request.
constructAppEnv :: IO En.AppEnv
constructAppEnv = do
    config  <- Cf.getConfig
    manager <- newManager tlsManagerSettings
    tRes    <- dieLeft =<< liftIO (acquireToken config manager)

    return En.AppEnv
        { En._accessToken = Just $ Ak.fromResponse tRes
        , En._config      = config
        , En._httpManager = manager
        }

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ unwords args
