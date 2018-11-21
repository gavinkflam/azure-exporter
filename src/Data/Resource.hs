{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions related to Azure resources.
module Data.Resource
    (
      -- * Resource
      parseResourceId
    , resourceId
    ) where

import Data.Text.Lazy (Text, intercalate, splitOn, toLower)

import qualified Data.ResourceMetadata as D

-- | Parse an Azure resource URI and breakdown into meaningful components.
parseResourceId :: Text -> D.ResourceMetadata
parseResourceId id = D.ResourceMetadata
    { D._resourceGroup    = s !! 4
    , D._resourceName     = s !! 8
    , D._resourceProvider = s !! 6
    , D._resourceType     = s !! 7
    , D._subscriptionId   = s !! 2
    }
  where
    s = splitOn "/" $ toLower id

-- | Extract the top level resource URI and standardize the resource URI.
--
--   The Azure API will response the resource URI in whatever cases we requested
--   with. Thus, we can only ensure the consistency by downcasing the resource
--   URI.
resourceId :: Text -> Text
resourceId id = intercalate "/" $ take 9 $ splitOn "/" $ toLower id
