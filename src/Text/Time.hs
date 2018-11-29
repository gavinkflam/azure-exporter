-- | Format `UTCTime` as text.
module Text.Time
    (
      -- * Time
      formatTime
    ) where

import Data.Time.Clock (UTCTime)
import qualified Data.Time.Format as F

-- | Format `UTCTime` with the given format using the default locale.
formatTime :: String -> UTCTime -> String
formatTime = F.formatTime F.defaultTimeLocale
