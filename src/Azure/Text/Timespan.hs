module Azure.Text.Timespan
  ( timespan
  , getTimespanFromNow
  ) where

import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

timespan :: UTCTime -> UTCTime -> String
timespan f t = iso8601UTCFormatTime f <> "/" <> iso8601UTCFormatTime t

getTimespanFromNow :: NominalDiffTime -> NominalDiffTime -> IO String
getTimespanFromNow fromOffset toOffset = do
  now <- getCurrentTime
  let from = addUTCTime (- fromOffset) now
      to   = addUTCTime (- toOffset) now
  return $ timespan from to

-- Formatting
iso8601UTCFormatTime :: UTCTime -> String
iso8601UTCFormatTime = formatTime defaultTimeLocale "%FT%H:%M:%SZ"
