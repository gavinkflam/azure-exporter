-- | Test timespan `String` construction from `UTCTime`.
module Text.TimespanSpec
    (
      -- * Spec
      spec
    ) where

import Test.Hspec

import qualified Data.Dummy.Time as T
import Text.Timespan

-- | Spec for `Timespan`.
spec :: Spec
spec = do
    describe "timespan" $
        it "formats a timespan string from two UTCTime as expected" $
            timespan T.timeFrom T.timeTo `shouldBe` T.dummyTimespan

    describe "timespanFrom" $
        it "formats a timespan string from an UTCTime and two offsets as expected" $
            timespanFrom T.timeTo 60 0 `shouldBe` T.dummyTimespan
