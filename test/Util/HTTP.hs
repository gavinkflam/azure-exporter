module Util.HTTP
    (
      -- * Request body
      parseSimpleRequestBody
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Network.HTTP.Client (RequestBody(..))
import Network.HTTP.Types (parseSimpleQuery)

-- | Parse URL encoded request body into `SimpleQuery`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS` containing URL encoded content.
parseSimpleRequestBody :: RequestBody -> [(ByteString, ByteString)]
parseSimpleRequestBody = parseSimpleQuery . toBS

-- | Extract `ByteString` from `RequestBody`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS`.
toBS :: RequestBody -> ByteString
toBS (RequestBodyLBS s) = toStrict s
toBS (RequestBodyBS  s) = s
toBS _                  = undefined :: ByteString
