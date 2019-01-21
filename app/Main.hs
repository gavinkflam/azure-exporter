module Main where

import App.Action.AppEnv (constructAppEnv)

import Server (runServer)

-- | Entry point for Azure exporter executable.
main :: IO ()
main = runServer =<< constructAppEnv

-- | Derive the unrecognized argument error message from arguments.
argsError :: [String] -> String
argsError args = "Unrecognized command " ++ unwords args
