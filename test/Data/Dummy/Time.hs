{-# LANGUAGE OverloadedStrings #-}

module Data.Dummy.Time
    (
      -- * Time
      timeFrom
    , timeTo
      -- * Timestamp
    , timestampFrom
    , timestampTo
      -- * Unix Timestamp
    , unixTimestampFrom
      -- * Timespan
    , dummyTimespan
    ) where

import Data.Text (Text, unpack)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

-- | Dummy time from (Tuesday, 26-Jun-18 08:00:00 UTC).
timeFrom :: UTCTime
timeFrom = posixSecondsToUTCTime 1530000000

-- | Dummy time to (Tuesday, 26-Jun-18 08:01:00 UTC).
timeTo :: UTCTime
timeTo = posixSecondsToUTCTime 1530000060

-- | Timestamp for `timeFrom` in ISO8601.
timestampFrom :: Text
timestampFrom = "2018-06-26T08:00:00Z"

-- | Timestamp for `timeTo` in ISO8601.
timestampTo :: Text
timestampTo = "2018-06-26T08:01:00Z"

-- | Unix timestamp for 26-Jun-18 08:00:00 UTC.
unixTimestampFrom :: Int
unixTimestampFrom = 1530000000

-- | Dummy timespan according to `timeFrom` and `timeTo`.
dummyTimespan :: String
dummyTimespan = unpack $ timestampFrom <> "/" <> timestampTo
