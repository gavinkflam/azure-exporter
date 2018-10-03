{-# LANGUAGE OverloadedStrings #-}

module AzureExporter.Util.Resource
  ( parseResourceId
  , resourceId
  ) where

import qualified AzureExporter.Data.ResourceMetadata as D
import           Data.Text.Lazy (Text, intercalate, splitOn, toLower)

parseResourceId :: Text -> D.ResourceMetadata
parseResourceId id =
  D.ResourceMetadata { D._resourceGroup    = s !! 4
                     , D._resourceName     = s !! 8
                     , D._resourceProvider = s !! 6
                     , D._resourceType     = s !! 7
                     , D._subscriptionId   = s !! 2
                     }
                       where s = splitOn "/" $ toLower id

-- The Azure API will response the resource ID in whatever cases we
-- requested with.
-- Thus, we can only ensure the consistency by downcasing the resource ID.
resourceId :: Text -> Text
resourceId id = intercalate "/" $ take 9 $ splitOn "/" $ toLower id
