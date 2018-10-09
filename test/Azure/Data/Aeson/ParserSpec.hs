{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test JSON decoding and error extraction mechanism.
module Azure.Data.Aeson.ParserSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Data.Aeson.Parser
import qualified Azure.Data.Error.ErrorResponse as E
import qualified Azure.Data.Error.ErrorValue as V
import qualified Azure.Data.Monitor.ListMetricValuesResponse as R
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Dummy.Text as T
import           Data.Text.Lazy (unpack)
import           Expectations
import           Test.Hspec

-- | Spec for `Parser`.
spec :: Spec
spec = do
  let fullMessage = unpack $ T.errorCode <> ": " <> T.errorMessage

  describe "errorExtractor" $ do
    it "extracts readable error message from ErrorResponse JSON ByteString" $
      errorExtractor T.errorResponseJSON `shouldSatisfy` isJustOf fullMessage

    it "extracts readable error message from ErrorValue JSON ByteString" $
      errorExtractor T.errorValueJSON `shouldSatisfy` isJustOf fullMessage

  describe "mapEitherDecode" $ do
    it "extracts readable error message from ErrorResponse JSON ByteString" $
      decodeLMVR T.errorResponseJSON `shouldSatisfy` isLeftOf fullMessage

    it "extracts readable error message from ErrorValue JSON ByteString" $
      decodeLMVR T.errorValueJSON `shouldSatisfy` isLeftOf fullMessage

    it "returns the original content for invalid error structure" $
      decodeLMVR "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String ListMetricValuesResponse`.
decodeLMVR :: ByteString -> Either String R.ListMetricValuesResponse
decodeLMVR = mapEitherDecode errorExtractor
