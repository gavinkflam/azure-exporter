{-# LANGUAGE TemplateHaskell #-}

module Data.Config
    (
      -- * Types
      Config
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
import Data.Text (Text, pack)
import System.Environment (getEnv, lookupEnv)

import Control.Lens (makeLenses)

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
    } deriving Show

makeLenses ''Config

-- | Construct configuration from environment variables.
getConfig :: IO Config
getConfig = do
    clientId'       <- pack <$> getEnv "CLIENT_ID"
    clientSecret'   <- pack <$> getEnv "CLIENT_SECRET"
    port'           <- read <$> getEnvWithDef "9492" "PORT"
    subscriptionId' <- pack <$> getEnv "SUBSCRIPTION_ID"
    tenantId'       <- pack <$> getEnv "TENANT_ID"
    offerId'        <- pack <$> getEnvWithDef "MS-AZR-0003p" "OFFER_ID"
    currency'       <- pack <$> getEnvWithDef "USD" "CURRENCY"
    locale'         <- pack <$> getEnvWithDef "en-US" "LOCALE"
    regionInfo'     <- pack <$> getEnvWithDef "US" "REGION_INFO"

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
getEnvWithDef :: String -> String -> IO String
getEnvWithDef def k = fromMaybe def <$> lookupEnv k
