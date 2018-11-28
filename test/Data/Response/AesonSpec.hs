{-# LANGUAGE OverloadedStrings #-}

-- | Test JSON decoding and error extraction mechanism.
module Data.Response.AesonSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS

import Data.Response.Aeson as R
import Test.Hspec

import qualified Data.Response.TestData as D
import Expectations
import Network.HTTP.Client.Internal (Response)
import Network.HTTP.Types (badRequest400, ok200)

-- | Spec for `Parser`.
spec :: Spec
spec = do
    describe "errorExtractor" $ do
        it "extracts readable error message from ErrorResponse JSON" $
            errorExtractor D.errorResponseJson
            `shouldSatisfy` isJustOf D.expectedFullErrorMessage

        it "extracts readable error message from ErrorValue JSON" $
            errorExtractor D.errorValueJson
            `shouldSatisfy` isJustOf D.expectedFullErrorMessage

    let sDecode = eitherDecode . D.response ok200
        fDecode = eitherDecode . D.response badRequest400

    describe "mapEitherDecode" $ do
        it "extracts the expected JsonValue from non-ErrorResponse JSON" $
            sDecode D.nonErrorResponseJson
            `shouldSatisfy` isRightOf D.expectedNonErrorResponseJsonValue

        it "returns the expected JSON deserialization error" $
            sDecode D.errorValueJson
            `shouldSatisfy` isLeftOf D.expectedJsonValueError

        it "extracts readable error message from ErrorResponse JSON" $
            fDecode D.errorResponseJson
            `shouldSatisfy` isLeftOf D.expectedFullErrorMessage

        it "extracts readable error message from ErrorValue JSON" $
            fDecode D.errorValueJson
            `shouldSatisfy` isLeftOf D.expectedFullErrorMessage

        it "returns the original content for invalid error structure" $
            fDecode "Kaboom!" `shouldSatisfy` isLeftOf "Kaboom!"

-- | Decoding with concrete type `Either String JsonValue`.
eitherDecode :: Response LBS.ByteString -> Either String D.JsonValue
eitherDecode = mapEitherDecode R.errorExtractor
