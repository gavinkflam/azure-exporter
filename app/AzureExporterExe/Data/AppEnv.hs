{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.AppEnv
  ( AppEnv (..)
  -- Lenses
  , accessToken
  , config
  , httpManager
  ) where

import AzureExporterExe.Data.AccessToken (AccessToken)
import AzureExporterExe.Data.Config (Config)
import Control.Lens (makeLenses)
import Network.HTTP.Client (Manager)

data AppEnv =
  AppEnv { _accessToken :: Maybe AccessToken
         , _config      :: Config
         , _httpManager :: Manager
         }

makeLenses ''AppEnv
