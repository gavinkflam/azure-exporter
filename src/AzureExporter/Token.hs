{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Token
  ( AcquireTokenParams
  , AcquireTokenResponse
  , acquireToken
  ) where

import           Control.Applicative (empty)
import           Control.Lens (makeLenses, (^.))
import           Data.Aeson
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)

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

acquireTokenForm :: AcquireTokenParams -> [(B.ByteString, B.ByteString)]
acquireTokenForm p =
  [ ("grant_type",    "client_credentials")
  , ("resource",      "https://management.azure.com/")
  , ("client_id",     encodeUtf8 $ p ^. clientId)
  , ("client_secret", encodeUtf8 $ p ^. clientSecret)
  ]

acquireToken :: AcquireTokenParams -> IO (Maybe AcquireTokenResponse)
acquireToken p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ acquireTokenUrl $ p ^. tenantId
  let req' = urlEncodedBody (acquireTokenForm p) $ req { method = "POST" }
  res <- httpLbs req' manager
  return $ decode $ responseBody res
