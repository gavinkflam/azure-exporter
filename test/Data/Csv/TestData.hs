{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.TestData
    (
      -- * Types
      DynamicRecord
      -- * Test data
    , testRecords
      -- * Result
    , expectedHeader
    , expectedCsv
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS

import Data.Csv (Header, ToNamedRecord, toNamedRecord)
import Data.Csv.ToHeader (ToHeader, comparison, header)
import Data.HashMap.Strict (HashMap, keys)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- | Data structure with variable header for each record.
newtype DynamicRecord = DynamicRecord (HashMap ByteString Int)

-- | Header deriving and sorting for `DynamicRecord`.
instance ToHeader DynamicRecord where
    header (DynamicRecord m) = keys m
    comparison _             = compare

-- | Named record deriving for `DynamicRecord`.
instance ToNamedRecord DynamicRecord where
    toNamedRecord (DynamicRecord m) = toNamedRecord m

-- | Records with variable header for each record.
testRecords :: [DynamicRecord]
testRecords = map (DynamicRecord . HM.fromList)
    [ [ ("a", 1)
      , ("x", 2)
      , ("c", 3)
      ]
    , [ ("b", 4)
      , ("d", 5)
      , ("e", 6)
      ]
    , [ ("a", 7)
      , ("e", 8)
      , ("y", 9)
      , ("f", 10)
      ]
    , [ ("y", 11)
      , ("x", 12)
      , ("f", 13)
      , ("e", 14)
      , ("d", 15)
      , ("c", 16)
      , ("b", 17)
      , ("a", 18)
      ]
    ]

-- | Expected header derived from `testRecords`.
--
--   It should be sorted and should not duplicate.
expectedHeader :: Header
expectedHeader = V.fromList ["a", "b", "c", "d", "e", "f", "x", "y"]

-- | Expected csv text derived from `testRecords`.
--
--   Some fields should be blank because of the variable header.
expectedCsv :: LBS.ByteString
expectedCsv = LBS.intercalate "\r\n"
    [ "a,b,c,d,e,f,x,y"
    , "1,,3,,,,2,"
    , ",4,,5,6,,,"
    , "7,,,,8,10,,9"
    , "18,17,16,15,14,13,12,11"
    , ""
    ]
