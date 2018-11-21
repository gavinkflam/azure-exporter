{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Data.Billing.AggregateProperty
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

import Data.Text (Text)

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)

import Data.Billing.InstanceData (InstanceData)

-- | AggregateProperty
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data AggregateProperty = AggregateProperty
    { _meterId          :: {-# UNPACK #-} !Text
    , _meterCategory    :: Maybe Text
    , _meterSubCategory :: Maybe Text
    , _meterName        :: Maybe Text
    , _meterRegion      :: Maybe Text
    , _subscriptionId   :: {-# UNPACK #-} !Text
    , _unit             :: Maybe Text
    , _usageStartTime   :: {-# UNPACK #-} !UTCTime
    , _usageEndTime     :: {-# UNPACK #-} !UTCTime
    , _instanceData     :: Maybe InstanceData
    , _quantity         :: {-# UNPACK #-} !Scientific
    } deriving (Show)

instance FromJSON AggregateProperty where
    parseJSON =
        withObject "AggregateProperty" $ \v -> AggregateProperty
            <$>  v .:  "meterId"
            <*>  v .:? "meterCategory"
            <*>  v .:? "meterSubCategory"
            <*>  v .:? "meterName"
            <*>  v .:? "meterRegion"
            <*>  v .:  "subscriptionId"
            <*>  v .:? "unit"
            <*>  v .:  "usageStartTime"
            <*>  v .:  "usageEndTime"
            <*> (v .:? "instanceData"
                >>= traverse (withEmbeddedJSON "InstanceData" parseJSON))
            <*>  v .:  "quantity"

makeLenses ''AggregateProperty
