{-# LANGUAGE TemplateHaskell #-}

module Data.AzureRm.ResourceMetadata
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

import Data.Text (Text)

import Control.Lens (makeLenses)

-- | Data structure for breaking down an Azure resource URI.
data ResourceMetadata = ResourceMetadata
    { _resourceGroup    :: {-# UNPACK #-} !Text
    , _resourceName     :: {-# UNPACK #-} !Text
    , _resourceProvider :: {-# UNPACK #-} !Text
    , _resourceType     :: {-# UNPACK #-} !Text
    , _subscriptionId   :: {-# UNPACK #-} !Text
    } deriving Show

makeLenses ''ResourceMetadata
