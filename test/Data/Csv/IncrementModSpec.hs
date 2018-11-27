-- | Test deriving Csv from records with variable header for each record.
module Data.Csv.IncrementModSpec
    (
      -- * Spec
      spec
    ) where

import Data.Csv.IncrementMod (encodeNamedRecords)
import Test.Hspec

import qualified Data.Csv.TestData as D

-- | Spec for `IncrementMod`.
spec :: Spec
spec =
    describe "encodeNamedRecords" $
        it "derive csv text from variable header records" $
            encodeNamedRecords D.testRecords `shouldBe` D.expectedCsv
