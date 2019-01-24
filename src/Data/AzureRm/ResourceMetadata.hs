module Data.AzureRm.ResourceMetadata
    (
      -- * Types
      ResourceMetadata (..)
    ) where

import Data.Text (Text)

-- | Data structure for breaking down an Azure resource URI.
data ResourceMetadata = ResourceMetadata
    { resourceGroup    :: {-# UNPACK #-} !Text
    , resourceName     :: {-# UNPACK #-} !Text
    , resourceProvider :: {-# UNPACK #-} !Text
    , resourceType     :: {-# UNPACK #-} !Text
    , subscriptionId   :: {-# UNPACK #-} !Text
    } deriving Show
