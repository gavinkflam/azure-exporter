-- | Test deriving unique and sorted header from records with variable header.
module Data.Csv.ToHeaderSpec
    (
      -- * Spec
      spec
    ) where

import Data.Csv.ToHeader (toHeader)
import Test.Hspec

import qualified Data.Csv.TestData as D

-- | Spec for `ToHeader`.
spec :: Spec
spec =
    describe "toHeader" $
        it "derive unique and sorted header from variable header records" $
            toHeader D.testRecords `shouldBe` D.expectedHeader
