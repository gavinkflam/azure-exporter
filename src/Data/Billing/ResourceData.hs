{-# LANGUAGE DeriveGeneric #-}

module Data.Billing.ResourceData
    (
      -- * Types
      ResourceData (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson
import Data.HashMap.Strict (HashMap)

import Data.Aeson.Options (aesonOptions)

-- | ResourceData
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data ResourceData = ResourceData
    { resourceUri    :: {-# UNPACK #-} !Text
    , location       :: {-# UNPACK #-} !Text
    , tags           :: Maybe (HashMap Text (Maybe Text))
    , additionalInfo :: Maybe (HashMap Text (Maybe Text))
    } deriving (Generic, Show)

instance FromJSON ResourceData where
    parseJSON = genericParseJSON aesonOptions
