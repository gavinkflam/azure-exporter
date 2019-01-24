{-# LANGUAGE DeriveGeneric #-}

module Data.Billing.GetRateCardResponse
    (
      -- * Types
      GetRateCardResponse (..)
    ) where

import Data.Text (Text)
import GHC.Generics

import Data.Aeson

import Data.Aeson.Options (capitalizeAesonOptions)
import Data.Billing.Meter (Meter)

-- | Response for list usage aggregates API.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)#response>
data GetRateCardResponse = GetRateCardResponse
    { meters        :: [Meter]
    , currency      :: {-# UNPACK #-} !Text
    , locale        :: {-# UNPACK #-} !Text
    , isTaxIncluded :: !Bool
    } deriving (Generic, Show)

instance FromJSON GetRateCardResponse where
    parseJSON = genericParseJSON capitalizeAesonOptions
