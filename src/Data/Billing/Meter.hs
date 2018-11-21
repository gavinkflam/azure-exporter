{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Billing.Meter
    (
      -- * Types
      Meter (..)
      -- * Lenses
    , meterId
    , meterName
    , meterCategory
    , meterSubCategory
    , meterRegion
    , meterRates
    , unit
    ) where

import Data.Text (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Scientific (Scientific)

import Data.Aeson.Options (capitalizeAesonOptions)

-- | Meter
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)#response>
data Meter = Meter
    { _meterId          :: {-# UNPACK #-} !Text
    , _meterName        :: {-# UNPACK #-} !Text
    , _meterCategory    :: {-# UNPACK #-} !Text
    , _meterSubCategory :: {-# UNPACK #-} !Text
    , _meterRegion      :: {-# UNPACK #-} !Text
    , _meterRates       :: HashMap Text Scientific
    , _unit             :: {-# UNPACK #-} !Text
    } deriving (Generic, Show)

instance FromJSON Meter where
    parseJSON = genericParseJSON capitalizeAesonOptions

makeLenses ''Meter
