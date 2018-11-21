{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

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

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (oAuth2AesonOptions)

-- | Azure OAuth 2.0 Service-to-Service access token response.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#service-to-service-access-token-response>
data AcquireAccessTokenResponse = AcquireAccessTokenResponse
    { _accessToken  :: {-# UNPACK #-} !Text
    , _expiresIn    :: {-# UNPACK #-} !Text
    , _expiresOn    :: {-# UNPACK #-} !Text
    , _extExpiresIn :: {-# UNPACK #-} !Text
    , _notBefore    :: {-# UNPACK #-} !Text
    , _resource     :: {-# UNPACK #-} !Text
    , _tokenType    :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON AcquireAccessTokenResponse where
    parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''AcquireAccessTokenResponse
