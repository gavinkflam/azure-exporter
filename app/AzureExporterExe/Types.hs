module AzureExporterExe.Types
  -- * Types
  ( AppAction
  ) where

import Control.Monad.AppEnvSTM (AppEnvSTM)
import Data.Text.Lazy (Text)
import Web.Scotty.Trans (ActionT)

-- |
-- Type for Scotty `ActionT` monad.
--
-- Error will be in `Text`.
--
-- Actions will be provided with the `AppEnv` via STM reader `AppEnvSTM`.
type AppAction a = ActionT Text AppEnvSTM a
