{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Config
  ( Config
  -- Lenses
  , clientId
  , clientSecret
  , subscriptionId
  , tenantId
  -- IO
  , getConfig
  ) where

import Control.Lens (makeLenses, (^.))
import Data.Text (Text, pack)
import System.Environment (getEnv)

data Config =
  Config { _clientId       :: Text
         , _clientSecret   :: Text
         , _subscriptionId :: Text
         , _tenantId       :: Text
         } deriving Show

makeLenses ''Config

getConfig :: IO Config
getConfig = do
  clientId       <- getEnv "CLIENT_ID"
  clientSecret   <- getEnv "CLIENT_SECRET"
  subscriptionId <- getEnv "SUBSCRIPTION_ID"
  tenantId       <- getEnv "TENANT_ID"

  return $ Config { _clientId       = pack clientId
                  , _clientSecret   = pack clientSecret
                  , _subscriptionId = pack subscriptionId
                  , _tenantId       = pack tenantId
                  }
