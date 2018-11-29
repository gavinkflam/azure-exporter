-- | Test formatting number in scientific notation.
module Text.ScientificSpec
    (
      -- * Spec
      spec
    ) where

import Data.Scientific (Scientific)
import Test.Hspec
import qualified Text.Scientific as Sc

-- | Spec for `Scientific`.
spec :: Spec
spec =
    describe "showFixed" $ do
        it "format short number as text" $
            Sc.showFixed testShortNumber `shouldBe` expectedShortNumberText
        it "format long number as text" $
            Sc.showFixed testLongNumber `shouldBe` expectedLongNumberText

-- | Short number in scientific notation to test for formatting.
testShortNumber :: Scientific
testShortNumber = read expectedShortNumberText

-- | Expected short number text in fixed point notation.
expectedShortNumberText :: String
expectedShortNumberText = "3.141"

-- | Long number in scientific notation to test for formatting.
testLongNumber :: Scientific
testLongNumber = read expectedLongNumberText

-- | Expected short number text in fixed point notation.
expectedLongNumberText :: String
expectedLongNumberText = "3.141592653589793238462643383279502884197169399375105"
