-- | Test formatting number in scientific notation.
module Text.TimeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Test.Hspec
import qualified Text.Time as Ti

-- | Spec for `Time`.
spec :: Spec
spec = do
    let f1 = Ti.formatTime "%F"
        f2 = Ti.formatTime "%FT%H:%M:%SZ"

    describe "formatTime" $ do
        it "formats time as ISO8601 date text" $
            f1 testTime `shouldBe` expectedDateText
        it "formats time as ISO8601 date and time text" $
            f2 testTime `shouldBe` expectedDateAndTimeText

-- | Test time for text formatting (Tuesday, 26-Jun-18 08:00:00 UTC).
testTime :: UTCTime
testTime = posixSecondsToUTCTime 1530000000

-- | Expected ISO8601 date text.
expectedDateText :: String
expectedDateText = "2018-06-26"

-- | Expected ISO8601 date and time text.
expectedDateAndTimeText :: String
expectedDateAndTimeText = "2018-06-26T08:00:00Z"
