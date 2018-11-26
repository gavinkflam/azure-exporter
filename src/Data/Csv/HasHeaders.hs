{-# LANGUAGE ScopedTypeVariables #-}

module Data.Csv.HasHeaders
    (
      -- * Types
      HasHeaders (..)
      -- * Foldable
    , uniqueHeaders
    ) where

import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Set (Set, toList, union)
import qualified Data.Set as Set

import Data.Csv (Header, Name)
import Data.Vector (fromList)

-- | Typeclass for data structure with dynamic headers.
class HasHeaders a where
    -- | Derive headers for each record.
    headers :: a -> [Name]
    -- | Traits type class which `a` value will always be undefined.
    comparison :: a -> Name -> Name -> Ordering

-- | Derive sorted unique headers for multiple `HasHeaders`.
uniqueHeaders :: forall a. HasHeaders a => [a] -> Header
uniqueHeaders = fromList . sortBy c . toList . headerSet
  where
    c = comparison (undefined :: a)

-- | Derive header `Set` for multiple `HasHeaders`.
headerSet :: HasHeaders a => [a] -> Set ByteString
headerSet = foldr (union . Set.fromList . headers) Set.empty
