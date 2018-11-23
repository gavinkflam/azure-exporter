module Data.Csv.HasHeaders
    (
      -- * Types
      HasHeaders (..)
      -- * Foldable
    , uniqueHeaders
    ) where

import Data.ByteString (ByteString)
import Data.Set (Set, toList, union)
import qualified Data.Set as Set

import Data.Csv (Header)
import Data.Vector (fromList)

-- | Typeclass for data structure with dynamic headers.
class HasHeaders a where
    headers :: a -> [ByteString]

-- | Derive the unique `Header` Vector for foldable `HasHeaders`.
uniqueHeaders :: (Foldable t, HasHeaders a) => t a -> Header
uniqueHeaders = fromList . toList . uniqueHeaders'

-- | Derive the unique set of headers for foldable `HasHeaders`.
uniqueHeaders' :: (Foldable t, HasHeaders a) => t a -> Set ByteString
uniqueHeaders' = foldr (union . Set.fromList . headers) Set.empty
