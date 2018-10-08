{-# LANGUAGE OverloadedStrings #-}

-- | Tests for error extractor.
module Azure.Control.Error.ExtractorSpec
-- * Spec
  ( spec
  ) where

import           Azure.Control.Error.Extractor
import qualified Azure.Data.Error.ErrorResponse as E
import qualified Azure.Data.Error.ErrorValue as V
import qualified Azure.Data.Monitor.ListMetricValuesResponse as R
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Expectations
import           Test.Hspec

-- |
-- Test JSON decoding and error extraction mechanism.
spec :: Spec
spec = do
  let fullMessage = errorCode <> ": " <> errorMessage

  describe "errorExtractor" $
    it "extracts error code and message" $
      errorExtractor errorResponse `shouldBe` fullMessage

  describe "mapEitherDecode" $ do
    it "extracts error code and message from ErrorResponse ByteString" $
      decodeLMVR errorJSON `shouldSatisfy` isLeftOf (unpack fullMessage)

    -- TODO: Implement fallback mechanism to extract from ErrorValue ByteString
    it "extracts error code and message from ErrorValue ByteString" $
      False

    it "returns the original content for invalid error structure" $
      decodeLMVR "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String ListMetricValuesResponse`.
decodeLMVR :: ByteString -> Either String R.ListMetricValuesResponse
decodeLMVR = mapEitherDecode errorExtractor

-- | Dummy error code.
errorCode :: Text
errorCode = "InvalidOperation"

-- | Dummy error message.
errorMessage :: Text
errorMessage = "The system is going to explode!"

-- | Dummy error response in JSON `ByteString`.
errorJSON :: ByteString
errorJSON = encodeUtf8 $
  "{" <>
    "\"error\": {" <>
      "\"code\": \"" <> errorCode <> "\"," <>
      "\"message\": \"" <> errorMessage <> "\"" <>
    "}" <>
  "}"

-- | Dummy `ErrorValue` item.
errorValue :: V.ErrorValue
errorValue =
  V.ErrorValue { V._code    = errorCode
               , V._message = errorMessage
               }

-- | Dummy `ErrorResponse` item.
errorResponse :: E.ErrorResponse
errorResponse =
  E.ErrorResponse { E.__error = errorValue
                  }
