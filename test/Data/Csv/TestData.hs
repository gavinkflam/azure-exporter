{-# LANGUAGE OverloadedStrings #-}

module Data.Csv.TestData
    (
      -- * Test data
      testRecords
    ) where

import qualified Data.HashMap.Strict as HM

import Data.Csv.DynamicRecord (DynamicRecord(..))

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
