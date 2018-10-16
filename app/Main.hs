module Main where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Exit (die)

import AzureExporterExe.Server (runServer)

-- | Entry point for Azure exporter executable.
main :: IO ()
main = do
  args <- getArgs
  case args of
    ["server"] -> runServer
    _          -> die $ argsError args

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ intercalate " " args
