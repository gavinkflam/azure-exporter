module AzureExporterExe.Control.Monad.Maybe
  -- * Exceptions
  ( raiseIfNothing
  ) where

import Web.Scotty.Trans (ActionT, ScottyError, raise, stringError)

-- | Raise the error message if `Nothing`, otherwise extract the `Just` value.
raiseIfNothing :: (ScottyError e, Monad m) => String -> Maybe a -> ActionT e m a
raiseIfNothing e Nothing  = raise $ stringError e
raiseIfNothing _ (Just x) = return x
