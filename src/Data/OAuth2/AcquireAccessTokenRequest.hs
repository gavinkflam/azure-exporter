{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

-- | Service-to-service access token request with a shared secret.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
module Data.OAuth2.AcquireAccessTokenRequest
    (
      -- * Types
      Params (..)
      -- * Request
    , request
      -- * Error
    , errorExtractor
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text, lines, stripEnd, unpack)
import Data.Text.Encoding (encodeUtf8)
import Prelude hiding (lines)

import Control.Lens (makeLenses, (^.))
import Data.Aeson (decode)
import Network.HTTP.Client (Request, parseRequest_, urlEncodedBody)

import Data.OAuth2.ErrorResponse (errorDescription)
import Data.Response.Aeson (ErrorHandler)

-- | Parameters to construct request.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
data Params = Params
    { _clientId     :: {-# UNPACK #-} !Text
    , _clientSecret :: {-# UNPACK #-} !Text
    , _tenantId     :: {-# UNPACK #-} !Text
    } deriving Show

makeLenses ''Params

-- | Construct URL from tenant ID.
url :: Text -> String
url tenantId' =
    "https://login.microsoftonline.com/" <> unpack tenantId' <> "/oauth2/token"

-- | Construct form parameters from params.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-client-creds-grant-flow#first-case-access-token-request-with-a-shared-secret>
form :: Params -> [(ByteString, ByteString)]
form p =
    [ ("grant_type",    "client_credentials")
    , ("resource",      "https://management.azure.com/")
    , ("client_id",     encodeUtf8 (p ^. clientId))
    , ("client_secret", encodeUtf8 (p ^. clientSecret))
    ]

-- | Construct request from params.
request :: Params -> Request
request p =
    urlEncodedBody (form p) $ parseRequest_ $ "POST " <> url (p ^. tenantId)

-- | Extract readable error message from `ErrorResponse` JSON `ByteString`.
errorExtractor :: ErrorHandler
errorExtractor =
    fmap (unpack . stripEnd . head . lines . (^. errorDescription)) . decode
