module AzureExporterExe.Control.Monad.Either
  ( raiseLeft
  , dieLeft
  ) where

import System.Exit (die)
import Web.Scotty.Trans (ActionT, ScottyError, raise, stringError)

-- Raise the error message from Left, otherwise extract value from Right
raiseLeft :: (ScottyError e, Monad m) => Either String a -> ActionT e m a
raiseLeft (Left e)  = raise $ stringError e
raiseLeft (Right n) = return n

-- Exit with error message from Left, otherwise extract value from Right
dieLeft :: Either String a -> IO a
dieLeft (Left m)  = die m
dieLeft (Right x) = return x
