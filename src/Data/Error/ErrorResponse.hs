{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Error.ErrorResponse
  (
  -- * Types
    ErrorResponse (..)
  -- * Lenses
  , _error
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Error.ErrorValue (ErrorValue)

-- | Azure REST API ErrorResponse object.
--
-- <https://github.com/Microsoft/api-guidelines/blob/master/Guidelines.md#errorresponse--object>
newtype ErrorResponse =
  ErrorResponse { __error :: ErrorValue
                } deriving (Generic, Show)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ErrorResponse
