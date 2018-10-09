module Data.Dummy.Time
  (
  -- * Time
    timeFrom
  , timeTo
  -- * Timespan
  , dummyTimespan
  ) where

import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Dummy time from (Tuesday, 26-Jun-18 08:00:00 UTC)
timeFrom :: UTCTime
timeFrom = posixSecondsToUTCTime 1530000000

-- | Dummy time to (Tuesday, 26-Jun-18 08:01:00 UTC)
timeTo :: UTCTime
timeTo = posixSecondsToUTCTime 1530000060

-- | Dummy timespan according to `timeFrom` and `timeTo`
dummyTimespan :: String
dummyTimespan = "2018-06-26T08:00:00Z/2018-06-26T08:01:00Z"
