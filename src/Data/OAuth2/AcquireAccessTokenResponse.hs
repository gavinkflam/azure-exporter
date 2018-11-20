{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.OAuth2.AcquireAccessTokenResponse
  (
  -- * Types
    AcquireAccessTokenResponse (..)
  -- * Lenses
  , accessToken
  , expiresIn
  , expiresOn
  , extExpiresIn
  , notBefore
  , resource
  , tokenType
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson
import Text.Casing (quietSnake)

import Data.Aeson.Options (oAuth2AesonOptions)

-- | Azure OAuth 2.0 Service-to-Service access token response.
--
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#service-to-service-access-token-response>
data AcquireAccessTokenResponse = AcquireAccessTokenResponse
  { _accessToken  :: Text
  , _expiresIn    :: Text
  , _expiresOn    :: Text
  , _extExpiresIn :: Text
  , _notBefore    :: Text
  , _resource     :: Text
  , _tokenType    :: Text
  } deriving (Generic, Show)

instance FromJSON AcquireAccessTokenResponse where
  parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''AcquireAccessTokenResponse
