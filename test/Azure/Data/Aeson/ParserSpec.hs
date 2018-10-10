{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test JSON decoding and error extraction mechanism.
module Azure.Data.Aeson.ParserSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Data.Aeson.Parser
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Dummy.Text as T
import           Data.JsonValue
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
    it "extracts JsonValue from JsonValue JSON ByteString" $
      eitherDecode T.jsonValueJSON `shouldSatisfy` isRightOf expectedJsonValue

    it "extracts readable error message from ErrorResponse JSON ByteString" $
      eitherDecode T.errorResponseJSON `shouldSatisfy` isLeftOf fullMessage

    it "extracts readable error message from ErrorValue JSON ByteString" $
      eitherDecode T.errorValueJSON `shouldSatisfy` isLeftOf fullMessage

    it "returns the original content for invalid error structure" $
      eitherDecode "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String JsonValue`.
eitherDecode :: ByteString -> Either String JsonValue
eitherDecode = mapEitherDecode errorExtractor

-- | Expected deserialized `JsonValue` item from JSON `ByteString`.
expectedJsonValue :: JsonValue
expectedJsonValue = JsonValue { _value = T.jsonValueValue }
