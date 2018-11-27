{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.ToHeader
    (
      -- * Types
      ToHeader (..)
      -- * Utility
    , toHeader
    ) where

import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Set (Set, union)
import qualified Data.Set as Set

import Data.Csv (Header, Name)
import qualified Data.Vector as V

-- | Typeclass for data structure with vairalbe header for each record.
class ToHeader a where
    -- | Derive header for each record.
    header :: a -> [Name]
    -- | Traits type class for sorting header.
    --   First value will always be `undefined`.
    comparison :: a -> Name -> Name -> Ordering

-- | Derive unique and sorted header for records.
toHeader :: forall a. ToHeader a => [a] -> Header
toHeader = V.fromList . sortBy c . Set.toList . headerSet
  where
    c = comparison (undefined :: a)

-- | Derive header set from records.
headerSet :: ToHeader a => [a] -> Set ByteString
headerSet = foldr (union . Set.fromList . header) Set.empty
