{-# LANGUAGE OverloadedStrings #-}

module Data.Response.TestData
    (
      -- * Test data
      errorResponseJson
    , errorValueJson
    , nonErrorResponseJson
    , response
      -- * Result
    , expectedFullErrorMessage
    , expectedNonErrorResponseJsonValue
    , expectedJsonValueError
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.JsonValue (JsonValue(..))
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types (Status, http11)

-- | Test `ErrorResponse` JSON text.
errorResponseJson :: LBS.ByteString
errorResponseJson = "{\"error\":" <> errorValueJson <> "}"

-- | Test `ErrorValue` JSON text.
errorValueJson :: LBS.ByteString
errorValueJson = LBS.fromStrict $ encodeUtf8 $ T.concat
    [ "{\"code\": \"" <> (testDatastore ! "errorCode") <> "\""
    , ",\"message\": \"" <> (testDatastore ! "errorMessage") <> "\""
    , "}"
    ]

-- | Test non-error response JSON text.
nonErrorResponseJson :: LBS.ByteString
nonErrorResponseJson = "{\"value\": 42}"

-- | Construct a response from status code and body.
response :: Status -> LBS.ByteString -> Response LBS.ByteString
response s b = Response
    { responseStatus = s
    , responseVersion = http11
    , responseHeaders = []
    , responseBody = b
    , responseCookieJar = mempty
    , responseClose' = ResponseClose $ return ()
    }

-- | Expected error message derived from `errorValueJson`.
expectedFullErrorMessage :: String
expectedFullErrorMessage = T.unpack $
    (testDatastore ! "errorCode") <> ": " <> (testDatastore ! "errorMessage")

-- | Expected `JsonValue` from `nonErrorResponseJson`.
expectedNonErrorResponseJsonValue :: JsonValue
expectedNonErrorResponseJsonValue = JsonValue { _value = 42 }

-- | Expected deserialization error for parsing `nonErrorResponseJson`.
expectedJsonValueError :: String
expectedJsonValueError = "Error in $: key \"value\" not present"

-- | Datastore for test data.
testDatastore :: HashMap Text Text
testDatastore = HM.fromList
    [ ("errorCode",    "InvalidOperation")
    , ("errorMessage", "The system is going to explode!")
    ]
