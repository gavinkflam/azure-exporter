{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Error.ErrorValue
    (
      -- * Types
      ErrorValue (..)
      -- * Lenses
    , code
    , message
    ) where

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)

-- | Azure REST API Error object.
--
--   <https://github.com/Microsoft/api-guidelines/blob/master/Guidelines.md#error--object>
data ErrorValue = ErrorValue
    { _code    :: {-# UNPACK #-} !Text
    , _message :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON ErrorValue where
    parseJSON = genericParseJSON aesonOptions

makeLenses ''ErrorValue
