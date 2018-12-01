{-# LANGUAGE OverloadedStrings #-}

-- | Test deriving unique and sorted header from records with variable header.
module Data.Csv.ToHeaderSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import Data.Csv.ToHeader (toHeader)
import Test.Hspec

import qualified Data.Csv.TestData as D
import qualified Data.Vector as V

-- | Spec for `ToHeader`.
spec :: Spec
spec =
    describe "toHeader" $
        it "derives unique and sorted header from variable header records" $
            toHeader D.testRecords `shouldBe` expectedHeader

-- | Expected header derived from `testRecords`.
--
--   It should be sorted and should not duplicate.
expectedHeader :: V.Vector ByteString
expectedHeader = V.fromList ["a", "b", "c", "d", "e", "f", "x", "y"]
