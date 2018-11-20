{-# LANGUAGE DeriveGeneric #-}

module Data.JsonValue
  (
  -- * Types
    JsonValue (..)
  ) where

import GHC.Generics

import Data.Aeson

-- | JsonValue type to test for JSON deserialization mechanism
newtype JsonValue = JsonValue
  { _value :: Int
  } deriving (Eq, Generic, Show)

instance FromJSON JsonValue where
  parseJSON = genericParseJSON options
    where options = defaultOptions { fieldLabelModifier = dropWhile (== '_') }
