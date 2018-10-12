{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Service-to-service access token request with a shared secret.
--
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
module Azure.Request.OAuth2.AcquireAccessToken
  (
  -- * Types
    Params (..)
  -- * Request
  , request
  -- * Error
  , errorExtractor
  ) where

import           Azure.Data.OAuth2.ErrorResponse (errorDescription)
import           Control.Lens (makeLenses, (^.))
import           Data.Aeson (decode)
import           Data.Text.Lazy (Text, lines, stripEnd, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString as BS
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Network.HTTP.Client (Request, parseRequest_, urlEncodedBody)
import           Prelude hiding (lines)

-- | Parameters to construct `Request`.
--
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
data Params =
  Params { _clientId     :: Text
         , _clientSecret :: Text
         , _tenantId     :: Text
         } deriving Show

makeLenses ''Params

-- | Construct URL from tenant ID.
url :: Text -> String
url tenantId =
  "https://login.microsoftonline.com/" <> unpack tenantId <> "/oauth2/token"

-- | Construct form parameters from `Params`.
--
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
form :: Params -> [(BS.ByteString, BS.ByteString)]
form p =
  [ ("grant_type",    "client_credentials")
  , ("resource",      "https://management.azure.com/")
  , ("client_id",     toStrict $ encodeUtf8 $ p ^. clientId)
  , ("client_secret", toStrict $ encodeUtf8 $ p ^. clientSecret)
  ]

-- | Construct `Request` from `Params`.
request :: Params -> Request
request p =
  urlEncodedBody (form p) $ parseRequest_ $ "POST " <> url (p ^. tenantId)

-- | Extract readable error message from `ErrorResponse` JSON `ByteString`.
errorExtractor :: ByteString -> Maybe String
errorExtractor =
  fmap (unpack . stripEnd . head . lines . (^. errorDescription)) . decode
