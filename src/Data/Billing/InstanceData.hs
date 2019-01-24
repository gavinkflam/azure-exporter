{-# LANGUAGE OverloadedStrings #-}

module Data.Billing.InstanceData
    (
    -- * Types
      InstanceData (..)
    ) where

import Data.Aeson

import Data.Billing.ResourceData (ResourceData)

-- | InstanceData
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
newtype InstanceData = InstanceData
    { resourceData :: ResourceData
    } deriving (Show)

instance FromJSON InstanceData where
    parseJSON =
        withObject "InstanceData" $ \v -> InstanceData
            <$> v .: "Microsoft.Resources"
