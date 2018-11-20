{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Billing.UsageAggregate
  (
  -- * Types
    UsageAggregate (..)
  -- * Lenses
  , _id
  , name
  , properties
  , _type
  ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (aesonOptions)
import Data.Billing.AggregateProperty (AggregateProperty)

-- | UsageAggregate
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data UsageAggregate = UsageAggregate
  { __id        :: Text
  , _name       :: Text
  , _properties :: AggregateProperty
  , __type      :: Text
  } deriving (Generic, Show)

instance FromJSON UsageAggregate where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''UsageAggregate
