{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the acquire access token API.
module Data.OAuth2.AcquireAccessTokenRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (unpack)

import Network.HTTP.Client (path, requestBody)
import Test.Hspec

import qualified Data.OAuth2.AcquireAccessTokenRequest as A
import qualified Data.OAuth2.TestData as D
import Expectations (isJustOf)
import Util.HTTP (parseSimpleRequestBody)

-- | Spec for `AcquireAccessToken`.
spec :: Spec
spec = do
    let rPath  = path D.acquireAccessTokenRequest
        fItems = parseSimpleRequestBody $ requestBody D.acquireAccessTokenRequest

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
