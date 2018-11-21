module Main where

import System.Environment (getArgs)
import System.Exit (die)

import Server (runServer)
import UsageDump (dumpUsage)

-- | Entry point for Azure exporter executable.
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["server"]             -> runServer
        ["dump-usage", t1, t2] -> dumpUsage t1 t2
        _                      -> die $ argsError args

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ unwords args
