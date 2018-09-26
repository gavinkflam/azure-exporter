{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Monitor.Data.LocalizableString
  ( LocalizableString (..)
  -- Lenses
  , value
  , localizedValue
  ) where

import AzureExporter.Monitor.Contract (aesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- LocalizableString
-- https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#localizablestring
data LocalizableString =
  LocalizableString { _value          :: Text
                    , _localizedValue :: Text
                    } deriving (Generic, Show)

instance FromJSON LocalizableString where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''LocalizableString
