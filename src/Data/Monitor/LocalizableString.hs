{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Monitor.LocalizableString
    (
      -- * Types
      LocalizableString (..)
      -- * Lenses
    , value
    , localizedValue
    ) where

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)

-- | LocalizableString
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#localizablestring>
data LocalizableString = LocalizableString
    { _value          :: {-# UNPACK #-} !Text
    , _localizedValue :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON LocalizableString where
    parseJSON = genericParseJSON aesonOptions

makeLenses ''LocalizableString
