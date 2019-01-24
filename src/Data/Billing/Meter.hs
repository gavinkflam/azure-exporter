{-# LANGUAGE DeriveGeneric #-}

module Data.Billing.Meter
    (
      -- * Types
      Meter (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)

import Data.Aeson.Options (capitalizeAesonOptions)

-- | Meter
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)#response>
data Meter = Meter
    { meterId          :: {-# UNPACK #-} !Text
    , meterName        :: {-# UNPACK #-} !Text
    , meterCategory    :: {-# UNPACK #-} !Text
    , meterSubCategory :: {-# UNPACK #-} !Text
    , meterRegion      :: {-# UNPACK #-} !Text
    , meterRates       :: HashMap Text Scientific
    , unit             :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON Meter where
    parseJSON = genericParseJSON capitalizeAesonOptions
