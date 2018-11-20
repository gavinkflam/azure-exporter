{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility functions related to the HTTP clientt.
module Text.HTTP
  (
  -- * Request
    addAuthHeader
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

import Network.HTTP.Client (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)

-- | Add the Authorization header to the `Request`.
addAuthHeader :: Text -> Request -> Request
addAuthHeader token req =
  req { requestHeaders = headers ++ [(hAuthorization, authValue)] }
    where headers   = requestHeaders req
          authValue = toStrict $ encodeUtf8 $ "Bearer " <> token
