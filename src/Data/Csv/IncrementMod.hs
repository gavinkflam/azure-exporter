{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.IncrementMod
    (
      -- * Encoding
      encodeNamedRecords
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromMaybe)

import Data.Csv (Header, NamedRecord, Record, ToNamedRecord, toNamedRecord)
import Data.Csv.Incremental (encode, encodeRecord)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Data.Csv.HasHeaders (HasHeaders, uniqueHeaders)

-- | Encode named records with support for varying headers.
encodeNamedRecords :: (ToNamedRecord a, HasHeaders a) => [a] -> ByteString
encodeNamedRecords xs =
    encode $ headerRow <> fBuild headers xs
  where
    headers   = uniqueHeaders xs
    headerRow = encodeRecord headers
    fBuild h  = foldr ((<>) . encodeRecord . toRecord h . toNamedRecord) mempty
    
-- | Convert `NamedRecord` to `Record`
toRecord :: Header -> NamedRecord -> Record
toRecord hs r = V.map f hs
  where
    f n = fromMaybe "" $ HM.lookup n r
