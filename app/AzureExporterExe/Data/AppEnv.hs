{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.AppEnv
  ( AppEnv (..)
  -- Lenses
  , config
  , accessToken
  ) where

import AzureExporterExe.Data.Config (Config)
import AzureExporterExe.Data.AccessToken (AccessToken)
import Control.Lens (makeLenses)

data AppEnv =
  AppEnv { _config      :: Config
         , _accessToken :: AccessToken
         } deriving Show

makeLenses ''AppEnv
