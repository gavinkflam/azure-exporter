{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Data.ResourceMetadata
  (
  -- * Types
    ResourceMetadata (..)
  -- * Lenses
  , resourceGroup
  , resourceName
  , resourceProvider
  , resourceType
  , subscriptionId
  ) where

import Control.Lens (makeLenses)
import Data.Text.Lazy (Text)

-- | Data structure for breaking down an Azure resource URI.
data ResourceMetadata =
  ResourceMetadata { _resourceGroup    :: Text
                   , _resourceName     :: Text
                   , _resourceProvider :: Text
                   , _resourceType     :: Text
                   , _subscriptionId   :: Text
                   } deriving Show

makeLenses ''ResourceMetadata
