{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.AppEnv
  -- * Types
  ( AppEnv (..)
  -- * Lenses
  , accessToken
  , config
  , httpManager
  ) where

import AzureExporterExe.Data.AccessToken (AccessToken)
import AzureExporterExe.Data.Config (Config)
import Control.Lens (makeLenses)
import Network.HTTP.Client (Manager)

-- | The shared application state for actions to persist cachable information.
data AppEnv =
  AppEnv { _accessToken :: Maybe AccessToken
         , _config      :: Config
         , _httpManager :: Manager
         }

makeLenses ''AppEnv
