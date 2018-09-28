{-# LANGUAGE OverloadedStrings #-}

module Azure.Contract
  ( aesonOptions
  , withAuth
  ) where

import Data.Aeson
import Data.ByteString (ByteString, append)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Client

-- Aeson options
aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropWhile (== '_') }

-- Request
withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> ByteString
authValue token = append "Bearer " $ toStrict $ encodeUtf8 token
