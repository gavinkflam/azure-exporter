module AzureExporterExe.Control.Scotty
  ( liftE
  ) where

import qualified Data.Text.Lazy as L
import           Web.Scotty (ActionM, raise)

-- Raise the error message from Left, otherwise extract value from Right
liftE :: ActionM (Either String a) -> ActionM a
liftE = flip (>>=) eitherRaise

eitherRaise :: Either String a -> ActionM a
eitherRaise (Left m)  = raise $ L.pack m
eitherRaise (Right n) = return n
