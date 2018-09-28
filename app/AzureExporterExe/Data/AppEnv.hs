{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.AppEnv
  ( AppEnv (..)
  -- Lenses
  , config
  , accessToken
  ) where

import AzureExporterExe.Data.AccessToken (AccessToken)
import AzureExporterExe.Data.Config (Config)
import Control.Lens (makeLenses)

data AppEnv =
  AppEnv { _config      :: Config
         , _accessToken :: Maybe AccessToken
         } deriving Show

makeLenses ''AppEnv
