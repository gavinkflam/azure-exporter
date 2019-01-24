{-# LANGUAGE OverloadedStrings #-}

module Data.Billing.AggregateProperty
    (
      -- * Types
      AggregateProperty (..)
    ) where

import Data.Text (Text)

import Data.Aeson
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)

import Data.Billing.InstanceData (InstanceData)

-- | AggregateProperty
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#json-element-definitions>
data AggregateProperty = AggregateProperty
    { meterId          :: {-# UNPACK #-} !Text
    , meterCategory    :: Maybe Text
    , meterSubCategory :: Maybe Text
    , meterName        :: Maybe Text
    , meterRegion      :: Maybe Text
    , subscriptionId   :: {-# UNPACK #-} !Text
    , unit             :: Maybe Text
    , usageStartTime   :: {-# UNPACK #-} !UTCTime
    , usageEndTime     :: {-# UNPACK #-} !UTCTime
    , instanceData     :: Maybe InstanceData
    , quantity         :: {-# UNPACK #-} !Scientific
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
