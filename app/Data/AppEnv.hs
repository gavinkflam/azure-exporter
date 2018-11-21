{-# LANGUAGE TemplateHaskell #-}

module Data.AppEnv
    (
      -- * Types
      AppEnv (..)
      -- * Lenses
    , accessToken
    , config
    , httpManager
    ) where

import Control.Lens (makeLenses)
import Network.HTTP.Client (Manager)

import Data.AccessToken (AccessToken)
import Data.Config (Config)

-- | The shared application state for actions to persist cachable information.
data AppEnv = AppEnv
    { _accessToken :: Maybe AccessToken
    , _config      :: Config
    , _httpManager :: Manager
    }

makeLenses ''AppEnv
