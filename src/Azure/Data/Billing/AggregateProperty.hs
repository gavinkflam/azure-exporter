{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.Billing.AggregateProperty
  (
  -- * Types
    AggregateProperty (..)
  -- * Lenses
  , meterId
  , meterCategory
  , meterSubCategory
  , meterName
  , meterRegion
  , subscriptionId
  , unit
  , usageStartTime
  , usageEndTime
  , instanceData
  , quantity
  ) where

import Azure.Data.Aeson.Options (aesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Text.Lazy (Text)
import GHC.Generics

-- | AggregateProperty
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data AggregateProperty =
  AggregateProperty { _meterId          :: Text
                    , _meterCategory    :: Maybe Text
                    , _meterSubCategory :: Maybe Text
                    , _meterName        :: Maybe Text
                    , _meterRegion      :: Maybe Text
                    , _subscriptionId   :: Text
                    , _unit             :: Maybe Text
                    , _usageStartTime   :: Text
                    , _usageEndTime     :: Text
                    , _instanceData     :: Maybe Text
                    , _quantity         :: Scientific
                    } deriving (Generic, Show)

instance FromJSON AggregateProperty where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''AggregateProperty
