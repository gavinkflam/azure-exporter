{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Control.Monad.Maybe
  ( raiseIfNothing
  ) where

import Web.Scotty.Trans (ActionT, ScottyError, raise, stringError)

-- Raise the error message if nothing, otherwise extract value
raiseIfNothing :: (ScottyError e, Monad m) => String -> Maybe a -> ActionT e m a
raiseIfNothing e Nothing  = raise $ stringError e
raiseIfNothing _ (Just x) = return x
