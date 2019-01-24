{-# LANGUAGE DeriveGeneric #-}

module Data.Error.ErrorResponse
    (
      -- * Types
      ErrorResponse (..)
    ) where

import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Error.ErrorValue (ErrorValue)

-- | Azure REST API ErrorResponse object.
--
--   <https://github.com/Microsoft/api-guidelines/blob/master/Guidelines.md#errorresponse--object>
newtype ErrorResponse = ErrorResponse
    { _error :: ErrorValue
    } deriving (Generic, Show)

instance FromJSON ErrorResponse where
    parseJSON = genericParseJSON aesonOptions
