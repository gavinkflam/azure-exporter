{-# LANGUAGE OverloadedStrings #-}

-- | Test timespan text construction.
module Text.Monitor.TimespanSpec
    (
      -- * Spec
      spec
    ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Test.Hspec
import qualified Text.Monitor.Timespan as Ts

-- | Spec for `Timespan`.
spec :: Spec
spec = do
    describe "timespan" $
        it "formats the expected timespan string from two UTCTime" $
            Ts.timespan timeFrom timeTo `shouldBe` expectedTimespan

    describe "timespanFrom" $
        it "formats the expected timespan string from UTCTime and two offsets" $
            Ts.timespanFrom timeTo 60 0 `shouldBe` expectedTimespan

-- | Test time for timespan text construction (Tuesday, 26-Jun-18 08:00:00 UTC).
timeFrom :: UTCTime
timeFrom = posixSecondsToUTCTime 1530000000

-- | Test time for timespan text construction (Tuesday, 26-Jun-18 08:01:00 UTC).
timeTo :: UTCTime
timeTo = posixSecondsToUTCTime 1530000060

-- | Expected timespan text constructed from `timeFrom` and `timeTo`
expectedTimespan :: String
expectedTimespan = "2018-06-26T08:00:00Z/2018-06-26T08:01:00Z"
