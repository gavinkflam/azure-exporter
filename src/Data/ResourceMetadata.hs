{-# LANGUAGE TemplateHaskell #-}

module Data.ResourceMetadata
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

import Data.Text.Lazy (Text)

import Control.Lens (makeLenses)

-- | Data structure for breaking down an Azure resource URI.
data ResourceMetadata = ResourceMetadata
  { _resourceGroup    :: Text
  , _resourceName     :: Text
  , _resourceProvider :: Text
  , _resourceType     :: Text
  , _subscriptionId   :: Text
  } deriving Show

makeLenses ''ResourceMetadata
