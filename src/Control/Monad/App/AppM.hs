module Control.Monad.App.AppM
    (
      -- * Types
      AppM
    ) where

import Control.Monad.Reader

import Data.App.AppEnv (AppEnv)

-- | Monad to read `AppEnv`.
type AppM = ReaderT AppEnv IO
