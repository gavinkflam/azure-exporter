{-# LANGUAGE DeriveGeneric #-}

module Data.OAuth2.AcquireAccessTokenResponse
    (
      -- * Types
      AcquireAccessTokenResponse (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (oAuth2AesonOptions)

-- | Azure OAuth 2.0 Service-to-Service access token response.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#service-to-service-access-token-response>
data AcquireAccessTokenResponse = AcquireAccessTokenResponse
    { accessToken  :: {-# UNPACK #-} !Text
    , expiresIn    :: {-# UNPACK #-} !Text
    , expiresOn    :: {-# UNPACK #-} !Text
    , extExpiresIn :: {-# UNPACK #-} !Text
    , notBefore    :: {-# UNPACK #-} !Text
    , resource     :: {-# UNPACK #-} !Text
    , tokenType    :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON AcquireAccessTokenResponse where
    parseJSON = genericParseJSON oAuth2AesonOptions
