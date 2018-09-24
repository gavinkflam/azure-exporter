{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Token
  ( AcquireTokenParams
  , AcquireTokenResponse
  , acquireToken
  ) where

import Control.Applicative (empty)
import Control.Lens (makeLenses, (^.), (^?))
import Data.Aeson
import Data.Text.Lazy (Text, unpack)
import Network.Wreq

data AcquireTokenParams =
  AcquireTokenParams { _clientId     :: Text
                     , _clientSecret :: Text
                     , _tenantId     :: Text
                     } deriving Show

makeLenses ''AcquireTokenParams

data AcquireTokenResponse =
  AcquireTokenResponse { _accessToken :: Text
                       , _expiresOn   :: Text
                       } deriving Show

instance FromJSON AcquireTokenResponse where
  parseJSON (Object v) =
    AcquireTokenResponse <$> v .: "access_token"
                         <*> v .: "expires_on"
  parseJSON _ = empty

makeLenses ''AcquireTokenResponse

acquireTokenUrl :: Text -> String
acquireTokenUrl tenantId =
  "https://login.microsoftonline.com/" <> unpack tenantId <> "/oauth2/token"

acquireTokenForm :: AcquireTokenParams -> [FormParam]
acquireTokenForm p =
  [ "grant_type"    := ("client_credentials" :: Text)
  , "resource"      := ("https://management.azure.com/" :: Text)
  , "client_id"     := p ^. clientId
  , "client_secret" := p ^. clientSecret
  ]

acquireToken :: AcquireTokenParams -> IO (Maybe AcquireTokenResponse)
acquireToken p = do
  r <- post (acquireTokenUrl $ p ^. tenantId) $ acquireTokenForm p
  return $ decode (r ^. responseBody)
