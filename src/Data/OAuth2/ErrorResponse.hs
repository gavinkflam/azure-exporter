{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.OAuth2.ErrorResponse
    (
      -- * Types
      ErrorResponse (..)
      -- * Lenses
    , correlationId
    , _error
    , errorCodes
    , errorDescription
    , timestamp
    , traceId
    ) where

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (oAuth2AesonOptions)

-- | Azure OAuth 2.0 credentials grant error response.
--
--   <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-on-behalf-of-flow#error-response-example>
data ErrorResponse = ErrorResponse
    { _correlationId    :: {-# UNPACK #-} !Text
    , __error           :: {-# UNPACK #-} !Text
    , _errorCodes       :: [Int]
    , _errorDescription :: {-# UNPACK #-} !Text
    , _timestamp        :: {-# UNPACK #-} !Text
    , _traceId          :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON ErrorResponse where
    parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''ErrorResponse
