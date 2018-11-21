{-# LANGUAGE OverloadedStrings #-}

-- | Test `Request` construction for the acquire access token API.
module Data.OAuth2.AcquireAccessTokenRequestSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Char8 as C
import Data.Text (unpack)
import Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client (path, requestBody)
import Test.Hspec

import qualified Data.Dummy.Text as T
import Data.OAuth2.AcquireAccessTokenRequest
import Expectations
import Util.HTTP (parseSimpleRequestBody)

-- | Spec for `AcquireAccessToken`.
spec :: Spec
spec = do
    let req    = request params
        fItems = parseSimpleRequestBody $ requestBody req

    describe "request" $ do
        it "contains grant_type form item" $
            fItems `shouldContain` [("grant_type", "client_credentials")]

        it "contains resource form item" $
            fItems `shouldContain` [("resource", "https://management.azure.com/")]

        it "contains client_id form item" $
            fItems `shouldContain` [("client_id", encodeUtf8 T.clientId)]

        it "contains client_secret form item" $
            fItems `shouldContain` [("client_secret", encodeUtf8 T.clientSecret)]

        it "contains the expected path" $
            C.unpack (path req) `shouldBe` expectedPath

    let err = errorExtractor T.oAuth2ErrorResponseJSON

    describe "errorExtractor" $ do
        it "extracts the first line of error descriptions" $
            err `shouldSatisfy` isJustOf (unpack $ head T.oAuth2ErrorDescriptionLines)

        it "returns nothing for invalid error structure" $
            errorExtractor T.jsonValueJSON `shouldBe` Nothing

-- | Dummy `Params` item.
params :: Params
params = Params
    { _clientId     = T.clientId
    , _clientSecret = T.clientSecret
    , _tenantId     = T.tenantId
    }

-- | The expected path should
--
-- #. Starts with the tenant ID
--
-- #. Follows by the API Endpoint
expectedPath :: String
expectedPath = unpack $ "/" <> T.tenantId <> "/oauth2/token"
