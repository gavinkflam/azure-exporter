{-# LANGUAGE OverloadedStrings #-}

module Azure.Monitor.Contract
  ( aesonOptions
  , withAuth
  ) where

import Data.Aeson
import Data.ByteString (ByteString, append)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client

-- Aeson options
aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = dropWhile (== '_') }

-- Request
withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> ByteString
authValue token = append "Bearer " $ encodeUtf8 token
