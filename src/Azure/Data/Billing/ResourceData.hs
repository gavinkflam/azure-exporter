{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.Billing.ResourceData
  (
  -- * Types
    ResourceData (..)
  -- * Lenses
  , resourceUri
  , location
  , tags
  , additionalInfo
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson
import Data.HashMap.Strict (HashMap)

import Azure.Data.Aeson.Options (aesonOptions)

-- | ResourceData
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data ResourceData =
  ResourceData { _resourceUri    :: Text
               , _location       :: Text
               , _tags           :: Maybe (HashMap Text Text)
               , _additionalInfo :: Maybe (HashMap Text Text)
               } deriving (Generic, Show)

instance FromJSON ResourceData where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ResourceData
