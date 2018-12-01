{-# LANGUAGE OverloadedStrings #-}

-- | Request utility for Azure RM API.
module Data.AzureRm.Request
    (
      -- * Request
      addAuthHeader
    ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client (Request, requestHeaders)
import Network.HTTP.Types.Header (hAuthorization)

-- | Add authorization header to request.
addAuthHeader :: Text -> Request -> Request
addAuthHeader token req = req
    { requestHeaders = headers ++ [(hAuthorization, authValue)]
    }
  where
    headers   = requestHeaders req
    authValue = encodeUtf8 $ "Bearer " <> token
