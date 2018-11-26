{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.IncrementMod
    (
      -- * Encoding
      encodeNamedRecords
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)

import Data.Csv (Header, NamedRecord, Record, ToNamedRecord, toNamedRecord)
import Data.Csv.Incremental (Builder, encode, encodeRecord)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Csv.HasHeaders (HasHeaders, uniqueHeaders)

-- | Encode named records with support for varying headers.
encodeNamedRecords :: (ToNamedRecord a, HasHeaders a) => [a] -> ByteString
encodeNamedRecords xs =
    encode $ builder headers headerRow xs
  where
    headers   = uniqueHeaders xs
    headerRow = encodeRecord headers
    
-- | Builder to add `ToNamedRecord` as `Record`.
builder :: ToNamedRecord a => Header -> Builder Record -> [a] -> Builder Record
builder hs =
    foldr (flip (<>) . encodeRecord . toRecord hs . toNamedRecord)

-- | Convert `NamedRecord` to `Record`
toRecord :: Header -> NamedRecord -> Record
toRecord hs r = V.map f hs
  where
    f n = fromMaybe "" $ HM.lookup n r
