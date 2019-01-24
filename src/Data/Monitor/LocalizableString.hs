{-# LANGUAGE DeriveGeneric #-}

module Data.Monitor.LocalizableString
    (
      -- * Types
      LocalizableString (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (aesonOptions)

-- | LocalizableString
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#localizablestring>
data LocalizableString = LocalizableString
    { value          :: {-# UNPACK #-} !Text
    , localizedValue :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON LocalizableString where
    parseJSON = genericParseJSON aesonOptions
