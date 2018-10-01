{-# LANGUAGE OverloadedStrings #-}

module AzureExporter.Util.Resource
  ( parseResourceId
  , resourceId
  ) where

import qualified AzureExporter.Data.ResourceMetadata as D
import           Data.Text.Lazy (Text, intercalate, splitOn)

parseResourceId :: Text -> D.ResourceMetadata
parseResourceId id =
  D.ResourceMetadata { D._resourceGroup    = s !! 4
                     , D._resourceName     = s !! 8
                     , D._resourceProvider = s !! 6
                     , D._resourceType     = s !! 7
                     , D._subscriptionId   = s !! 2
                     }
                       where s = splitOn "/" id

resourceId :: Text -> Text
resourceId id = intercalate "/" $ take 9 $ splitOn "/" id
