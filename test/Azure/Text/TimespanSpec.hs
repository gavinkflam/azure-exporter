-- |
-- Test timespan `String` construction from `UTCTime`.
module Azure.Text.TimespanSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Text.Timespan
import qualified Data.Dummy.Time as T
import           Expectations
import           Test.Hspec

-- | Spec for `Timespan`.
spec :: Spec
spec = do
  describe "timespan" $
    it "formats a timespan string from two UTCTime as expected" $
      timespan T.timeFrom T.timeTo `shouldBe` T.dummyTimespan

  describe "timespanFrom" $
    it "formats a timespan string from an UTCTime and two offsets as expected" $
      timespanFrom T.timeTo 60 0 `shouldBe` T.dummyTimespan
