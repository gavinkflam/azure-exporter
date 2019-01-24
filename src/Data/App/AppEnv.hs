module Data.App.AppEnv
    (
      -- * Types
      AppEnv (..)
    ) where

import Control.Concurrent.STM.TVar (TVar)
import Network.HTTP.Client (Manager)

import Data.App.AccessToken (AccessToken)
import Data.App.Config (Config)

-- | The shared application state for actions to persist cachable information.
data AppEnv = AppEnv
    { accessToken :: {-# UNPACK #-} !(TVar AccessToken)
    , config      :: {-# UNPACK #-} !Config
    , httpManager :: {-# UNPACK #-} !Manager
    }
