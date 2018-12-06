module Main where

import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.Reader (runReaderT)
import Data.Tuple.Curry (uncurryN)
import System.Environment (getArgs)
import System.Exit (die)

import App.Action.Gauge (dumpGauges)
import qualified App.Arg.Billing as Arb
import qualified App.Http.Billing as Htb
import Control.Monad.App.AppM (AppM)
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Server (runServer)

-- | Entry point for Azure exporter executable.
main :: IO ()
main = do
    appEnv <- constructAppEnv
    args   <- getArgs

    case args of
        ["server"]             -> runServer appEnv
        ["dump-usage", t1, t2] -> runReaderT (dumpBillingGauges t1 t2) appEnv
        _                      -> die $ argsError args

-- | Dump billing gauges to stdout.
dumpBillingGauges :: String -> String -> AppM ()
dumpBillingGauges t1 t2 =
    dumpGauges =<< uncurryN Htb.fetchGauges =<< Arb.assembleArgs t1 t2

-- | Construct `AppEnv` from environment variables and making HTTP request.
constructAppEnv :: IO En.AppEnv
constructAppEnv = do
    config  <- Cf.getConfig
    manager <- newManager tlsManagerSettings
    var     <- newTVarIO undefined

    return En.AppEnv
        { En._accessToken = var
        , En._config      = config
        , En._httpManager = manager
        }

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ unwords args
