module Main where

import Control.Monad.Reader (runReaderT)
import Data.Tuple.Curry (uncurryN)
import System.Environment (getArgs)
import System.Exit (die)

import App.Action.AppEnv (constructAppEnv)
import App.Action.Gauge (dumpGauges)
import qualified App.Arg.Billing as Arb
import qualified App.Http.Billing as Htb
import Control.Monad.App.AppM (AppM)

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

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ unwords args
