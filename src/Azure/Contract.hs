{-# LANGUAGE OverloadedStrings #-}

-- |
-- The constants for Azure API.
module Azure.Contract
  (
  -- * Versions
    monitorApiVersion
  -- * Request
  , withAuth
  ) where

import Data.ByteString (ByteString, append)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Client (Request, requestHeaders)

-- | The API version for Azure monitor.
monitorApiVersion :: Text
monitorApiVersion = "2018-01-01"

withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> ByteString
authValue token = append "Bearer " $ toStrict $ encodeUtf8 token
