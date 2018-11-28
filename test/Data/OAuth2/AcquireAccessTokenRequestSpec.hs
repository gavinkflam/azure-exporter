{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the acquire access token API.
module Data.OAuth2.AcquireAccessTokenRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Text (unpack)

import qualified Data.OAuth2.AcquireAccessTokenRequest as A
import Network.HTTP.Client (Request, RequestBody(..), path, requestBody)
import Network.HTTP.Types (parseSimpleQuery)
import Test.Hspec

import qualified Data.OAuth2.TestData as D
import Expectations (isJustOf)

-- | Spec for `AcquireAccessToken`.
spec :: Spec
spec = do
    let rPath  = path D.acquireAccessTokenRequest
        fItems = simpleRequestBody D.acquireAccessTokenRequest

    describe "request" $ do
        it "contains the expected grant type form item" $
            fItems `shouldContain` [D.queryItem "grant_type"]

        it "contains the expected resource form item" $
            fItems `shouldContain` [D.queryItem "resource"]

        it "contains the expected client id form item" $
            fItems `shouldContain` [D.queryItem "client_id"]

        it "contains the expected client secret form item" $
            fItems `shouldContain` [D.queryItem "client_secret"]

        it "contains the expected path" $
            rPath `shouldBe` D.expectedAcquireAccessTokenPath

    let err    = A.errorExtractor D.errorResponseJson
        nonErr = A.errorExtractor D.nonErrorResponseJson

    describe "errorExtractor" $ do
        it "extracts the first line of error descriptions" $
            err `shouldSatisfy` isJustOf (unpack $ head D.errorDescriptionLines)

        it "returns nothing for invalid error structure" $
            nonErr `shouldBe` Nothing

-- | Extract URL encoded request body into `SimpleQuery`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS` containing URL encoded content.
simpleRequestBody :: Request -> [(ByteString, ByteString)]
simpleRequestBody = parseSimpleQuery . toBS . requestBody

-- | Extract `ByteString` from `RequestBody`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS`.
toBS :: RequestBody -> ByteString
toBS (RequestBodyLBS s) = toStrict s
toBS (RequestBodyBS  s) = s
toBS _                  = undefined :: ByteString
