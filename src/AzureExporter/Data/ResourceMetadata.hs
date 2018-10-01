{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Data.ResourceMetadata
  ( ResourceMetadata (..)
  -- Lenses
  , resourceGroup
  , resourceName
  , resourceProvider
  , resourceType
  , subscriptionId
  ) where

import Control.Lens (makeLenses)
import Data.Text.Lazy (Text)

data ResourceMetadata =
  ResourceMetadata { _resourceGroup    :: Text
                   , _resourceName     :: Text
                   , _resourceProvider :: Text
                   , _resourceType     :: Text
                   , _subscriptionId   :: Text
                   } deriving Show

makeLenses ''ResourceMetadata
