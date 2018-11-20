-- |
-- Utility to format timespan `String` from `UTCTime`
module Text.Timespan
  (
  -- * Timespan
    timespan
  , timespanFrom
  ) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

-- | Render timespan `String` from start `UTCTime` and end `UTCTime`.
timespan :: UTCTime -> UTCTime -> String
timespan f t = iso8601UTCFormatTime f <> "/" <> iso8601UTCFormatTime t

-- |
-- Render timespan `String` from a reference `UTCTime`, the start time offset
-- and end time offset in `NominalDiffTime`.
timespanFrom :: UTCTime -> NominalDiffTime -> NominalDiffTime -> String
timespanFrom t fromOffset toOffset =
  timespan (fPast fromOffset) (fPast toOffset)
    where fPast offset = addUTCTime (negate $ abs offset) t

-- | Format a `UTCTime` with ISO8601 combined date and time format in UTC.
iso8601UTCFormatTime :: UTCTime -> String
iso8601UTCFormatTime = formatTime defaultTimeLocale "%FT%H:%M:%SZ"
