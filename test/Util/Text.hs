-- |
-- Utility functions for `Text`, `String` and `ByteString`.
module Util.Text
  (
  -- * ByteString
    toBS
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (encodeUtf8)

-- | Encode a Lazy `Text` to a strict `ByteString`
toBS :: Text -> ByteString
toBS = toStrict . encodeUtf8
