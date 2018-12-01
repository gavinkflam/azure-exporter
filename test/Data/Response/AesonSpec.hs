{-# LANGUAGE OverloadedStrings #-}

-- | Test JSON decoding and error extraction mechanism.
module Data.Response.AesonSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Data.Response.Aeson as R
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types (Status, badRequest400, http11, ok200)
import Test.Hspec

import Data.Response.JsonValue (JsonValue(..))
import qualified Data.Response.JsonValue as Jv
import Test.Expectations

-- | Spec for `Parser`.
spec :: Spec
spec = do
    describe "errorExtractor" $ do
        it "extracts readable error message from ErrorResponse JSON" $
            errorExtractor errorResponseJson
            `shouldSatisfy` isJustOf expectedFullErrorMessage

        it "extracts readable error message from ErrorValue JSON" $
            errorExtractor errorValueJson
            `shouldSatisfy` isJustOf expectedFullErrorMessage

    let sDecode = eitherDecode . response ok200
        fDecode = eitherDecode . response badRequest400

    describe "mapEitherDecode" $ do
        it "extracts the expected JsonValue from non-ErrorResponse JSON" $
            sDecode nonErrorResponseJson
            `shouldSatisfy` isRightOf expectedNonErrorResponseJsonValue

        it "returns the expected JSON deserialization error" $
            sDecode errorValueJson
            `shouldSatisfy` isLeftOf expectedJsonValueError

        it "extracts readable error message from ErrorResponse JSON" $
            fDecode errorResponseJson
            `shouldSatisfy` isLeftOf expectedFullErrorMessage

        it "extracts readable error message from ErrorValue JSON" $
            fDecode errorValueJson
            `shouldSatisfy` isLeftOf expectedFullErrorMessage

        it "returns the original content for invalid error structure" $
            fDecode "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String JsonValue`.
eitherDecode :: Response LBS.ByteString -> Either String JsonValue
eitherDecode = mapEitherDecode R.errorExtractor

-- | Test `ErrorResponse` JSON text.
errorResponseJson :: LBS.ByteString
errorResponseJson = "{\"error\":" <> errorValueJson <> "}"

-- | Test `ErrorValue` JSON text.
errorValueJson :: LBS.ByteString
errorValueJson = LBS.fromStrict $ encodeUtf8 $ T.concat
    [ "{\"code\": \"" <> (testTexts ! "errorCode") <> "\""
    , ",\"message\": \"" <> (testTexts ! "errorMessage") <> "\""
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
    (testTexts ! "errorCode") <> ": " <> (testTexts ! "errorMessage")

-- | Expected `JsonValue` from `nonErrorResponseJson`.
expectedNonErrorResponseJsonValue :: JsonValue
expectedNonErrorResponseJsonValue = JsonValue { Jv._value = 42 }

-- | Expected deserialization error for parsing `nonErrorResponseJson`.
expectedJsonValueError :: String
expectedJsonValueError = "Error in $: key \"value\" not present"

-- | Datastore for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("errorCode",    "InvalidOperation")
    , ("errorMessage", "The system is going to explode!")
    ]
