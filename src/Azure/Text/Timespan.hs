module Azure.Text.Timespan
  ( timespan
  , getLastMinuteTimespan
  ) where

import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

timespan :: (UTCTime, UTCTime) -> String
timespan (f, t) = iso8601UTCFormatTime f <> "/" <> iso8601UTCFormatTime t

getLastMinuteTimespan :: IO String
getLastMinuteTimespan = do
  now <- getCurrentTime
  return $ timespan (addUTCTime (-60) now, now)

-- Formatting
iso8601UTCFormatTime :: UTCTime -> String
iso8601UTCFormatTime = formatTime defaultTimeLocale "%FT%H:%M:%SZ"
