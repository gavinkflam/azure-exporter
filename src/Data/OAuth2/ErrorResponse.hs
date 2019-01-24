{-# LANGUAGE DeriveGeneric #-}

module Data.OAuth2.ErrorResponse
    (
      -- * Types
      ErrorResponse (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (oAuth2AesonOptions)

-- | Azure OAuth 2.0 credentials grant error response.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-on-behalf-of-flow#error-response-example>
data ErrorResponse = ErrorResponse
    { correlationId    :: {-# UNPACK #-} !Text
    , _error           :: {-# UNPACK #-} !Text
    , errorCodes       :: [Int]
    , errorDescription :: {-# UNPACK #-} !Text
    , timestamp        :: {-# UNPACK #-} !Text
    , traceId          :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON ErrorResponse where
    parseJSON = genericParseJSON oAuth2AesonOptions
