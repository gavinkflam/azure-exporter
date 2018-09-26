{-# LANGUAGE OverloadedStrings #-}

module AzureExporter.Monitor.Contract
  ( aesonOptions
  , withAuth
  ) where

import           Data.Aeson
import qualified Data.ByteString as B
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client

-- Aeson options
aesonOptions :: Options
aesonOptions = defaultOptions { fieldLabelModifier = drop 1 }

-- Request
withAuth :: Text -> Request -> Request
withAuth token req =
  req { requestHeaders = [("Authorization" , authValue token)] }

authValue :: Text -> B.ByteString
authValue token = B.append "Bearer " $ encodeUtf8 token
