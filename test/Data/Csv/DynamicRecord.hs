module Data.Csv.DynamicRecord
    (
      -- * Types
      DynamicRecord(..)
    ) where

import Data.ByteString (ByteString)

import Data.Csv (ToNamedRecord, toNamedRecord)
import Data.Csv.ToHeader (ToHeader, comparison, header)
import Data.HashMap.Strict (HashMap, keys)

-- | Data structure with variable header for each record.
newtype DynamicRecord =
    DynamicRecord (HashMap ByteString Int) deriving (Eq, Show)

-- | Header deriving and sorting for `DynamicRecord`.
instance ToHeader DynamicRecord where
    header (DynamicRecord m) = keys m
    comparison _             = compare

-- | Named record deriving for `DynamicRecord`.
instance ToNamedRecord DynamicRecord where
    toNamedRecord (DynamicRecord m) = toNamedRecord m
