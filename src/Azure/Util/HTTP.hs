{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility functions related to the HTTP clientt.
module Azure.Util.HTTP
  (
  -- * Request
    setAuthHeader
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Client (Request, requestHeaders)

-- | Set the Authorization header of the `Request` with the token provided.
setAuthHeader :: Text -> Request -> Request
setAuthHeader token req =
  req { requestHeaders = [("Authorization" , authValue)] }
    where authValue = toStrict $ encodeUtf8 $ "Bearer " <> token
