module AzureExporterExe.Control.Scotty
  ( liftE
  ) where

import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT, ScottyError, raise, stringError)

-- Raise the error message from Left, otherwise extract value from Right
liftE :: (ScottyError e, Monad m) => ActionT e m (Either String a) -> ActionT e m a
liftE = flip (>>=) eitherRaise

eitherRaise :: (ScottyError e, Monad m) => Either String a -> ActionT e m a
eitherRaise (Left e)  = raise $ stringError e
eitherRaise (Right n) = return n
