module Data.App.AccessToken
    (
      -- * Types
      AccessToken (..)
      -- * Utilities
    , fromResponse
    ) where

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import qualified Data.OAuth2.AcquireAccessTokenResponse as R

-- | Type representing an Azure OAuth2 access token.
data AccessToken = AccessToken
    { token     :: {-# UNPACK #-} !Text
    , expiresOn :: {-# UNPACK #-} !UTCTime
    } deriving (Eq, Show)

-- | Derive `AccessToken` from `AcquireAccessTokenResponse`.
fromResponse :: R.AcquireAccessTokenResponse -> AccessToken
fromResponse r = AccessToken
    { token     = R.accessToken r
    , expiresOn = parseTimestampText $ R.expiresOn r
    }

-- | Parse the UNIX timestamp text into `UTCTime`.
parseTimestampText :: Text -> UTCTime
parseTimestampText = posixSecondsToUTCTime . fromInteger . read . unpack
