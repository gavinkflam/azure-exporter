{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test JSON decoding and error extraction mechanism.
module Azure.Data.Aeson.ParserSpec
  (
  -- * Spec
    spec
  ) where

import           Data.ByteString.Lazy (ByteString)
import           Data.Monoid (mempty)
import           Data.Text.Lazy (unpack)

import           Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import           Network.HTTP.Types (Status, badRequest400, http11, ok200)
import           Test.Hspec

import           Azure.Data.Aeson.Parser
import qualified Data.Dummy.Text as T
import           Data.JsonValue
import           Expectations

-- | Spec for `Parser`.
spec :: Spec
spec = do
  let fullMessage = unpack $ T.errorCode <> ": " <> T.errorMessage

  describe "errorExtractor" $ do
    it "extracts readable error message from ErrorResponse JSON ByteString" $
      errorExtractor T.errorResponseJSON `shouldSatisfy` isJustOf fullMessage

    it "extracts readable error message from ErrorValue JSON ByteString" $
      errorExtractor T.errorValueJSON `shouldSatisfy` isJustOf fullMessage

  let sEitherDecode b = eitherDecode $ resp ok200 b
      fEitherDecode b = eitherDecode $ resp badRequest400 b

  describe "mapEitherDecode" $ do
    it "extracts JsonValue from JsonValue JSON ByteString" $
      sEitherDecode T.jsonValueJSON `shouldSatisfy` isRightOf expectedJsonValue

    it "returns the JSON deserialization error" $
      sEitherDecode T.errorValueJSON `shouldSatisfy` isLeftOf T.jsonValueError

    it "extracts readable error message from ErrorResponse JSON ByteString" $
      fEitherDecode T.errorResponseJSON `shouldSatisfy` isLeftOf fullMessage

    it "extracts readable error message from ErrorValue JSON ByteString" $
      fEitherDecode T.errorValueJSON `shouldSatisfy` isLeftOf fullMessage

    it "returns the original content for invalid error structure" $
      fEitherDecode "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String JsonValue`.
eitherDecode :: Response ByteString -> Either String JsonValue
eitherDecode = mapEitherDecode errorExtractor

-- | Expected deserialized `JsonValue` item from JSON `ByteString`.
expectedJsonValue :: JsonValue
expectedJsonValue = JsonValue { _value = T.jsonValueValue }

-- | Construct a response from a status code and `ByteString` body.
resp :: Status -> ByteString -> Response ByteString
resp s b =
  Response { responseStatus = s
           , responseVersion = http11
           , responseHeaders = []
           , responseBody = b
           , responseCookieJar = mempty
           , responseClose' = ResponseClose $ return ()
           }
