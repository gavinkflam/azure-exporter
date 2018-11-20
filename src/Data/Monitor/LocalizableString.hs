{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Monitor.LocalizableString
  (
  -- * Types
    LocalizableString (..)
  -- * Lenses
  , value
  , localizedValue
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)

-- | LocalizableString
--
-- <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#localizablestring>
data LocalizableString = LocalizableString
  { _value          :: Text
  , _localizedValue :: Text
  } deriving (Generic, Show)

instance FromJSON LocalizableString where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''LocalizableString
