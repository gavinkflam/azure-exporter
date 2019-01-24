{-# LANGUAGE DeriveGeneric #-}

module Data.Error.ErrorValue
    (
      -- * Types
      ErrorValue (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (aesonOptions)

-- | Azure REST API Error object.
--
--   <https://github.com/Microsoft/api-guidelines/blob/master/Guidelines.md#error--object>
data ErrorValue = ErrorValue
    { code    :: {-# UNPACK #-} !Text
    , message :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON ErrorValue where
    parseJSON = genericParseJSON aesonOptions
