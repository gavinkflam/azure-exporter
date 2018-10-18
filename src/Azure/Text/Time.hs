-- |
-- Utility to format `UTCTime` to `String`.
module Azure.Text.Time
  (
  -- * Time
    formatTime
  ) where

import           Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as F

-- | Format a `UTCTime` with the given format.
formatTime :: String -> UTCTime -> String
formatTime = F.formatTime F.defaultTimeLocale
