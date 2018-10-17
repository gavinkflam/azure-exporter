{-# LANGUAGE OverloadedStrings #-}
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

import Data.Text.Lazy (Text)

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Scientific (Scientific)
import Data.Time.Clock (UTCTime)

import Azure.Data.Aeson.Options (aesonOptions)
import Azure.Data.Billing.InstanceData (InstanceData)

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
                    , _usageStartTime   :: UTCTime
                    , _usageEndTime     :: UTCTime
                    , _instanceData     :: Maybe InstanceData
                    , _quantity         :: Scientific
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
