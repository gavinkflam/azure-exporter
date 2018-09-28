{-# LANGUAGE TemplateHaskell #-}

module AzureExporterExe.Data.AccessToken
  ( AccessToken (..)
  -- Lenses
  , accessToken
  , expiresOn
  -- Utilities
  , fromResponse
  ) where

import qualified Azure.Data.OAuth2.AcquireAccessTokenResponse as R
import           Control.Lens (makeLenses, (^.))
import           Data.Text.Lazy (Text, unpack)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

data AccessToken =
  AccessToken { _accessToken :: Text
              , _expiresOn   :: UTCTime
              } deriving Show

makeLenses ''AccessToken

fromResponse :: R.AcquireAccessTokenResponse -> AccessToken
fromResponse r =
  AccessToken { _accessToken = r ^. R.accessToken
              , _expiresOn   = parseTimestampText $ r ^. R.expiresOn
              }

parseTimestampText :: Text -> UTCTime
parseTimestampText t = posixSecondsToUTCTime $ fromInteger $ read $ unpack t
