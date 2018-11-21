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
import Text.Read (readMaybe)

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
    clientId'       <- getEnv "CLIENT_ID"
    clientSecret'   <- getEnv "CLIENT_SECRET"
    port'           <- readEnv 9492 "PORT"
    subscriptionId' <- getEnv "SUBSCRIPTION_ID"
    tenantId'       <- getEnv "TENANT_ID"
    offerId'        <- getEnv' "MS-AZR-0003p" "OFFER_ID"
    currency'       <- getEnv' "USD" "CURRENCY"
    locale'         <- getEnv' "en-US" "LOCALE"
    regionInfo'     <- getEnv' "US" "REGION_INFO"

    return Config
        { _clientId       = pack clientId'
        , _clientSecret   = pack clientSecret'
        , _port           = port'
        , _subscriptionId = pack subscriptionId'
        , _tenantId       = pack tenantId'
        , _offerId        = pack offerId'
        , _currency       = pack currency'
        , _locale         = pack locale'
        , _regionInfo     = pack regionInfo'
        }

-- | Get an environment variable with a fallback.
getEnv' :: String -> String -> IO String
getEnv' def k = fromMaybe def <$> lookupEnv k

-- | Get and parse an environment variable with a fallback.
readEnv :: Read a => a -> String -> IO a
readEnv def k = do
    v <- lookupEnv k
    return $ fromMaybe def $ readMaybe =<< v
