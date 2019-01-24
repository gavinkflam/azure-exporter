module Data.App.Config
    (
      -- * Types
      Config(..)
      -- * Configurations
    , getConfig
    ) where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Monad.System.MonadEnv (MonadEnv, getEnv, lookupEnv)

-- | Configurations for the application.
data Config = Config
    { clientId       :: {-# UNPACK #-} !Text
    , clientSecret   :: {-# UNPACK #-} !Text
    , port           :: {-# UNPACK #-} !Int
    , subscriptionId :: {-# UNPACK #-} !Text
    , tenantId       :: {-# UNPACK #-} !Text
    , offerId        :: {-# UNPACK #-} !Text
    , currency       :: {-# UNPACK #-} !Text
    , locale         :: {-# UNPACK #-} !Text
    , regionInfo     :: {-# UNPACK #-} !Text
    } deriving (Eq, Show)

-- | Construct configuration from environment variables.
getConfig :: MonadEnv m => m Config
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
        { clientId       = clientId'
        , clientSecret   = clientSecret'
        , port           = port'
        , subscriptionId = subscriptionId'
        , tenantId       = tenantId'
        , offerId        = offerId'
        , currency       = currency'
        , locale         = locale'
        , regionInfo     = regionInfo'
        }

-- | Get an environment variable with a fallback default.
getEnvDefault :: MonadEnv m => String -> String -> m String
getEnvDefault def k = fromMaybe def <$> lookupEnv k
