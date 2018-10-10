{-# LANGUAGE DeriveGeneric #-}

module Data.JsonValue
  (
  -- * Types
    JsonValue (..)
  ) where

import Data.Aeson
import GHC.Generics

-- | JsonValue type to test for JSON deserialization mechanism
newtype JsonValue =
  JsonValue { _value :: Int
            } deriving (Eq, Generic, Show)

instance FromJSON JsonValue where
  parseJSON = genericParseJSON options
    where options = defaultOptions { fieldLabelModifier = dropWhile (== '_') }
