{-# LANGUAGE OverloadedStrings #-}

module Azure.Contract
  ( withAuth
  ) where

import Data.ByteString (ByteString, append)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Client (Request, requestHeaders)

-- Request
withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> ByteString
authValue token = append "Bearer " $ toStrict $ encodeUtf8 token
