{-# LANGUAGE TemplateHaskell #-}

module Data.App.Config
    (
      -- * Types
      Config(..)
      -- * Lenses
    , clientId
    , clientSecret
    , port
    , subscriptionId
    , tenantId
    , offerId
    , currency
    , locale
    , regionInfo
      -- * Configurations
    , getConfig
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens (makeLenses)

import Control.Monad.System.EnvM (EnvM, getEnv, lookupEnv)

-- | Configurations for the application.
data Config = Config
    { _clientId       :: {-# UNPACK #-} !Text
    , _clientSecret   :: {-# UNPACK #-} !Text
    , _port           :: {-# UNPACK #-} !Int
    , _subscriptionId :: {-# UNPACK #-} !Text
    , _tenantId       :: {-# UNPACK #-} !Text
    , _offerId        :: {-# UNPACK #-} !Text
    , _currency       :: {-# UNPACK #-} !Text
    , _locale         :: {-# UNPACK #-} !Text
    , _regionInfo     :: {-# UNPACK #-} !Text
    } deriving (Eq, Show)

makeLenses ''Config

-- | Construct configuration from environment variables.
getConfig :: EnvM Config
getConfig = do
    clientId'       <- T.pack <$> getEnv "CLIENT_ID"
    clientSecret'   <- T.pack <$> getEnv "CLIENT_SECRET"
    port'           <- read <$> getEnvDefault "9492" "PORT"
    subscriptionId' <- T.pack <$> getEnv "SUBSCRIPTION_ID"
    tenantId'       <- T.pack <$> getEnv "TENANT_ID"
    offerId'        <- T.pack <$> getEnvDefault "MS-AZR-0003p" "OFFER_ID"
    currency'       <- T.pack <$> getEnvDefault "USD" "CURRENCY"
    locale'         <- T.pack <$> getEnvDefault "en-US" "LOCALE"
    regionInfo'     <- T.pack <$> getEnvDefault "US" "REGION_INFO"

    return Config
        { _clientId       = clientId'
        , _clientSecret   = clientSecret'
        , _port           = port'
        , _subscriptionId = subscriptionId'
        , _tenantId       = tenantId'
        , _offerId        = offerId'
        , _currency       = currency'
        , _locale         = locale'
        , _regionInfo     = regionInfo'
        }

-- | Get an environment variable with a fallback default.
getEnvDefault :: String -> String -> EnvM String
getEnvDefault def k = fromMaybe def <$> lookupEnv k
