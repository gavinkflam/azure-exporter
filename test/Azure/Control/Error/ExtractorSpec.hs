{-# LANGUAGE OverloadedStrings #-}

module Azure.Control.Error.ExtractorSpec
  ( spec
  ) where

import           Azure.Control.Error.Extractor
import qualified Azure.Data.Error.ErrorResponse as E
import qualified Azure.Data.Error.ErrorValue as V
import qualified Azure.Data.Monitor.ListMetricValuesResponse as R
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (Text, unpack)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Test.Hspec

spec :: Spec
spec = do
  let fullMessage = errorCode <> ": " <> errorMessage

  describe "errorExtractor" $
    it "extracts error code and message" $
      errorExtractor errorResponse `shouldBe` fullMessage

  describe "mapEitherDecode" $ do
    it "extracts error code and message from ByteString" $
      decodeLMVR errorJSON `shouldSatisfy` isLeftOf (unpack fullMessage)
    it "returns the original content for invalid error structure" $
      decodeLMVR "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

decodeLMVR :: ByteString -> Either String R.ListMetricValuesResponse
decodeLMVR = mapEitherDecode errorExtractor

isLeftOf :: String -> Either String a -> Bool
isLeftOf expected (Left s) = expected == s
isLeftOf _ (Right _)       = False

-- Data
errorCode :: Text
errorCode = "InvalidOperation"

errorMessage :: Text
errorMessage = "The system is going to explode!"

errorJSON :: ByteString
errorJSON = encodeUtf8 $
  "{" <>
    "\"error\": {" <>
      "\"code\": \"" <> errorCode <> "\"," <>
      "\"message\": \"" <> errorMessage <> "\"" <>
    "}" <>
  "}"

errorValue :: V.ErrorValue
errorValue =
  V.ErrorValue { V._code    = errorCode
               , V._message = errorMessage
               }

errorResponse :: E.ErrorResponse
errorResponse =
  E.ErrorResponse { E.__error = errorValue
                  }
