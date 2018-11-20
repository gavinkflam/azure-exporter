{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Billing.ListUsageAggregatesResponse
  (
  -- * Types
    ListUsageAggregatesResponse (..)
  -- * Lenses
  , value
  , nextLink
  -- * Query
  , continuationToken
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Types.URI (parseSimpleQuery)

import Data.Aeson.Options (aesonOptions)
import Data.Billing.UsageAggregate (UsageAggregate)
import Data.Monitor.Metric (Metric)

-- | Response for list usage aggregates API.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data ListUsageAggregatesResponse =
  ListUsageAggregatesResponse { _value    :: [UsageAggregate]
                              , _nextLink :: Maybe Text
                              } deriving (Generic, Show)

instance FromJSON ListUsageAggregatesResponse where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ListUsageAggregatesResponse

-- | Extract continuation token from `ListUsageAggregatesResponse`.
continuationToken :: ListUsageAggregatesResponse -> Maybe Text
continuationToken res =
  case _nextLink res of
    Nothing  -> Nothing
    Just url -> (decodeUtf8 . fromStrict) <$> lookup "continuationToken" q
      where q = parseSimpleQuery $ toStrict $ encodeUtf8 url
