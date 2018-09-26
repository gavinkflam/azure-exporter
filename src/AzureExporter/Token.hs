{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Token
  ( AcquireTokenParams (..)
  , AcquireTokenResponse (..)
  -- Lenses
  , accessToken
  , expiresIn
  , expiresOn
  , extExpiresIn
  , notBefore
  , resource
  , tokenType
  -- Request
  , acquireToken
  ) where

import           Control.Applicative (empty)
import           Control.Lens (makeLenses, (^.))
import           Data.Aeson
import           Data.Text (Text, unpack)
import           Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString as B
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Text.Casing (quietSnake)

data AcquireTokenParams =
  AcquireTokenParams { _clientId     :: Text
                     , _clientSecret :: Text
                     , _tenantId     :: Text
                     } deriving Show

makeLenses ''AcquireTokenParams

data AcquireTokenResponse =
  AcquireTokenResponse { _accessToken  :: Text
                       , _expiresIn    :: Text
                       , _expiresOn    :: Text
                       , _extExpiresIn :: Text
                       , _notBefore    :: Text
                       , _resource     :: Text
                       , _tokenType    :: Text
                       } deriving (Generic, Show)

instance FromJSON AcquireTokenResponse where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = quietSnake . drop 1 }

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
