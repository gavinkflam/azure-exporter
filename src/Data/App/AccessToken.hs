{-# LANGUAGE TemplateHaskell #-}

module Data.App.AccessToken
    (
      -- * Types
      AccessToken (..)
      -- * Lenses
    , token
    , expiresOn
      -- * Utilities
    , fromResponse
    ) where

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Control.Lens (makeLenses, (^.))

import qualified Data.OAuth2.AcquireAccessTokenResponse as R

-- | Type representing an Azure OAuth2 access token.
data AccessToken = AccessToken
    { _token     :: {-# UNPACK #-} !Text
    , _expiresOn :: {-# UNPACK #-} !UTCTime
    } deriving Show

makeLenses ''AccessToken

-- | Extract and construct the `AccessToken` from `AcquireAccessTokenResponse`.
fromResponse :: R.AcquireAccessTokenResponse -> AccessToken
fromResponse r = AccessToken
    { _token     = r ^. R.accessToken
    , _expiresOn = parseTimestampText (r ^. R.expiresOn)
    }

-- | Parse the UNIX timestamp in `Text` into `UTCTime`.
parseTimestampText :: Text -> UTCTime
parseTimestampText = posixSecondsToUTCTime . fromInteger . read . unpack
