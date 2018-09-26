{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.OAuth2.AcquireAccessToken
  ( Params (..)
  -- Request
  , acquireAccessToken
  ) where

import AzureExporter.OAuth2.Data.AcquireAccessTokenResponse (AcquireAccessTokenResponse)
import Control.Lens (makeLenses, (^.))
import Data.Aeson (decode)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- Request parameters
data Params =
  Params { _clientId     :: Text
         , _clientSecret :: Text
         , _tenantId     :: Text
         } deriving Show

makeLenses ''Params

-- Request utilities
acquireTokenUrl :: Text -> String
acquireTokenUrl tenantId =
  "https://login.microsoftonline.com/" <> unpack tenantId <> "/oauth2/token"

acquireTokenForm :: Params -> [(ByteString, ByteString)]
acquireTokenForm p =
  [ ("grant_type",    "client_credentials")
  , ("resource",      "https://management.azure.com/")
  , ("client_id",     encodeUtf8 $ p ^. clientId)
  , ("client_secret", encodeUtf8 $ p ^. clientSecret)
  ]

-- Request
acquireAccessToken :: Params -> IO (Maybe AcquireAccessTokenResponse)
acquireAccessToken p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ acquireTokenUrl $ p ^. tenantId

  let req' = urlEncodedBody (acquireTokenForm p) $ req { method = "POST" }
  res <- httpLbs req' manager
  return $ decode $ responseBody res
