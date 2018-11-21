-- | Utility to format `Scientific` to `String`.
module Text.Scientific
    (
      -- * Scientific
      showFixed
    ) where

import Data.Scientific (FPFormat(Fixed), Scientific, formatScientific)

-- | Format a `Scientific` in standard decimal notation.
showFixed :: Scientific -> String
showFixed = formatScientific Fixed Nothing
