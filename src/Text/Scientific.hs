-- | Format number in scientific notation as text.
module Text.Scientific
    (
      -- * Scientific
      showFixed
    ) where

import Data.Scientific (FPFormat(Fixed), Scientific, formatScientific)

-- | Format number in standard decimal notation.
showFixed :: Scientific -> String
showFixed = formatScientific Fixed Nothing
