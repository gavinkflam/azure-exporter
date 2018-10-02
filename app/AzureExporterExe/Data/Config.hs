{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.Config
  ( Config
  -- Lenses
  , clientId
  , clientSecret
  , port
  , subscriptionId
  , tenantId
  -- IO
  , getConfig
  ) where

import Control.Lens (makeLenses, (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text, pack)
import System.Environment (getEnv, lookupEnv)
import Text.Read (readMaybe)

data Config =
  Config { _clientId       :: Text
         , _clientSecret   :: Text
         , _port           :: Int
         , _subscriptionId :: Text
         , _tenantId       :: Text
         } deriving Show

makeLenses ''Config

getConfig :: IO Config
getConfig = do
  clientId       <- getEnv "CLIENT_ID"
  clientSecret   <- getEnv "CLIENT_SECRET"
  mPort          <- maybe Nothing readMaybe <$> lookupEnv "PORT"
  subscriptionId <- getEnv "SUBSCRIPTION_ID"
  tenantId       <- getEnv "TENANT_ID"

  return Config { _clientId       = pack clientId
                , _clientSecret   = pack clientSecret
                , _port           = fromMaybe 9492 mPort
                , _subscriptionId = pack subscriptionId
                , _tenantId       = pack tenantId
                }
