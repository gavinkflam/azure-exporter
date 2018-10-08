{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.OAuth2.ErrorResponse
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

import Azure.Contract (oAuth2AesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- | Azure OAuth 2.0 credentials grant error response.
--
-- <https://docs.microsoft.com/en-us/azure/active-directory/develop/v1-oauth2-on-behalf-of-flow#error-response-example>
data ErrorResponse =
  ErrorResponse { _correlationId    :: Text
                , __error           :: Text
                , _errorCodes       :: [Int]
                , _errorDescription :: Text
                , _timestamp        :: Text
                , _traceId          :: Text
                } deriving (Generic, Show)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''ErrorResponse
