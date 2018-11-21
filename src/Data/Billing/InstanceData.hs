{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Billing.InstanceData
    (
    -- * Types
      InstanceData (..)
    -- * Lenses
    , resourceData
    ) where

import Data.Text.Lazy (Text)

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Billing.ResourceData (ResourceData)

-- | InstanceData
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
newtype InstanceData = InstanceData
    { _resourceData :: ResourceData
    } deriving (Show)

instance FromJSON InstanceData where
    parseJSON =
        withObject "InstanceData" $ \v -> InstanceData
            <$> v .: "Microsoft.Resources"

makeLenses ''InstanceData
