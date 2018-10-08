{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test JSON decoding and error extraction mechanism.
module Azure.Control.Error.ExtractorSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Control.Error.Extractor
import qualified Azure.Data.Error.ErrorResponse as E
import qualified Azure.Data.Error.ErrorValue as V
import qualified Azure.Data.Monitor.ListMetricValuesResponse as R
import           Data.ByteString.Lazy (ByteString)
import           Data.Text.Lazy (unpack)
import qualified DummyText as T
import           Expectations
import           Test.Hspec

-- | Spec for `Extractor`.
spec :: Spec
spec = do
  let fullMessage = T.errorCode <> ": " <> T.errorMessage

  describe "errorExtractor" $
    it "extracts error code and message" $
      errorExtractor errorResponse `shouldBe` fullMessage

  describe "mapEitherDecode" $ do
    it "extracts error code and message from ErrorResponse ByteString" $
      decodeLMVR T.errorJSON `shouldSatisfy` isLeftOf (unpack fullMessage)

    -- TODO: Implement fallback mechanism to extract from ErrorValue ByteString
    it "extracts error code and message from ErrorValue ByteString" $
      False

    it "returns the original content for invalid error structure" $
      decodeLMVR "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String ListMetricValuesResponse`.
decodeLMVR :: ByteString -> Either String R.ListMetricValuesResponse
decodeLMVR = mapEitherDecode errorExtractor

-- | Dummy `ErrorValue` item.
errorValue :: V.ErrorValue
errorValue =
  V.ErrorValue { V._code    = T.errorCode
               , V._message = T.errorMessage
               }

-- | Dummy `ErrorResponse` item.
errorResponse :: E.ErrorResponse
errorResponse =
  E.ErrorResponse { E.__error = errorValue
                  }
