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
    { _correlationId    :: Text
    , __error           :: Text
    , _errorCodes       :: [Int]
    , _errorDescription :: Text
    , _timestamp        :: Text
    , _traceId          :: Text
    } deriving (Generic, Show)

instance FromJSON ErrorResponse where
    parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''ErrorResponse
