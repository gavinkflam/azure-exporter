{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Data.Billing.ListUsageAggregatesResponse
    (
      -- * Types
      ListUsageAggregatesResponse (..)
      -- * Query
    , continuationToken
    ) where

import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics

import Data.Aeson
import Network.HTTP.Types.URI (parseSimpleQuery)

import Data.Aeson.Options (aesonOptions)
import Data.Billing.UsageAggregate (UsageAggregate)

-- | Response for list usage aggregates API.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data ListUsageAggregatesResponse = ListUsageAggregatesResponse
    { value    :: [UsageAggregate]
    , nextLink :: Maybe Text
    } deriving (Generic, Show)

instance FromJSON ListUsageAggregatesResponse where
    parseJSON = genericParseJSON aesonOptions

-- | Extract continuation token from `ListUsageAggregatesResponse`.
continuationToken :: ListUsageAggregatesResponse -> Maybe Text
continuationToken res =
    case nextLink res of
        Nothing  -> Nothing
        Just url ->
            decodeUtf8 <$> lookup "continuationToken" q
          where
            q = parseSimpleQuery $ encodeUtf8 url
