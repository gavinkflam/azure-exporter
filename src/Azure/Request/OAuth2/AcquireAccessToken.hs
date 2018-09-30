{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Request.OAuth2.AcquireAccessToken
  ( Params (..)
  -- Request
  , acquireAccessToken
  ) where

import Azure.Control.Error.Extractor (mapEitherDecode)
import Azure.Data.OAuth2.AcquireAccessTokenResponse (AcquireAccessTokenResponse)
import Azure.Data.OAuth2.ErrorResponse (ErrorResponse, errorDescription)
import Control.Lens (makeLenses, (^.))
import Data.Text.Lazy (Text, lines, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude hiding (lines)

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
  , ("client_id",     toStrict $ encodeUtf8 $ p ^. clientId)
  , ("client_secret", toStrict $ encodeUtf8 $ p ^. clientSecret)
  ]

-- Request
acquireAccessToken :: Params -> IO (Either String AcquireAccessTokenResponse)
acquireAccessToken p = do
  manager <- newManager tlsManagerSettings
  req <- parseRequest $ acquireTokenUrl $ p ^. tenantId

  let req' = urlEncodedBody (acquireTokenForm p) $ req { method = "POST" }
  res <- httpLbs req' manager
  return $ mapEitherDecode errorExtractor $ responseBody res

errorExtractor :: ErrorResponse -> Text
errorExtractor = head . lines . (^. errorDescription)
