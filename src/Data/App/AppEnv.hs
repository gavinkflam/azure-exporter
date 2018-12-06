{-# LANGUAGE TemplateHaskell #-}

module Data.App.AppEnv
    (
      -- * Types
      AppEnv (..)
      -- * Lenses
    , accessToken
    , config
    , httpManager
    ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Lens (makeLenses)
import Network.HTTP.Client (Manager)

import Data.App.AccessToken (AccessToken)
import Data.App.Config (Config)

-- | The shared application state for actions to persist cachable information.
data AppEnv = AppEnv
    { _accessToken :: {-# UNPACK #-} !(TVar AccessToken)
    , _config      :: {-# UNPACK #-} !Config
    , _httpManager :: {-# UNPACK #-} !Manager
    }

makeLenses ''AppEnv
