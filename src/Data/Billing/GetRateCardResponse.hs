{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Data.Billing.GetRateCardResponse
    (
      -- * Types
      GetRateCardResponse (..)
      -- * Lenses
    , meters
    , currency
    , locale
    , isTaxIncluded
    ) where

import Data.Text.Lazy (Text)
import GHC.Generics

import Control.Lens (makeLenses)
import Data.Aeson

import Data.Aeson.Options (capitalizeAesonOptions)
import Data.Billing.Meter (Meter)

-- | Response for list usage aggregates API.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)#response>
data GetRateCardResponse = GetRateCardResponse
    { _meters        :: [Meter]
    , _currency      :: Text
    , _locale        :: Text
    , _isTaxIncluded :: Bool
    } deriving (Generic, Show)

instance FromJSON GetRateCardResponse where
    parseJSON = genericParseJSON capitalizeAesonOptions

makeLenses ''GetRateCardResponse
