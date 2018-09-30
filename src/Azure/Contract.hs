{-# LANGUAGE OverloadedStrings #-}

module Azure.Contract
  ( aesonOptions
  , oAuth2AesonOptions
  , withAuth
  ) where

import Data.Aeson
import Data.ByteString (ByteString, append)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Network.HTTP.Client
import Text.Casing (quietSnake)

-- Aeson options
aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = dropWhile (== '_') }

oAuth2AesonOptions :: Options
oAuth2AesonOptions =
  defaultOptions { fieldLabelModifier = quietSnake . dropWhile (== '_') }

-- Request
withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> ByteString
authValue token = append "Bearer " $ toStrict $ encodeUtf8 token
