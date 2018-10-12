{-# LANGUAGE OverloadedStrings #-}

-- |
-- Test `Request` construction for the acquire access token API.
module Azure.Request.OAuth2.AcquireAccessTokenSpec
  (
  -- * Spec
    spec
  ) where

import           Azure.Request.OAuth2.AcquireAccessToken
import qualified Data.ByteString.Char8 as C
import qualified Data.Dummy.Text as T
import           Data.Text.Lazy (unpack)
import           Expectations
import           Network.HTTP.Client (path, requestBody)
import           Test.Hspec
import           Util.HTTP (parseSimpleRequestBody)
import           Util.Text (toBS)

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
      fItems `shouldContain` [("client_id", toBS T.clientId)]

    it "contains client_secret form item" $
      fItems `shouldContain` [("client_secret", toBS T.clientSecret)]

    it "contains the expected path" $
      C.unpack (path req) `shouldBe` expectedPath

  let err = errorExtractor T.oAuth2ErrorResponseJSON

  describe "errorExtractor" $ do
    it "extracts the first line of error descriptions" $
      err `shouldSatisfy` isJustOf (unpack $ head T.oAuth2ErrorDescriptionLines)

    it "returns nothing for invalid error structure" $
      errorExtractor T.jsonValueJSON `shouldBe` Nothing

-- | Dummy `Params` item.
params =
  Params { _clientId     = T.clientId
         , _clientSecret = T.clientSecret
         , _tenantId     = T.tenantId
         }

-- |
-- The expected path should
--
-- 1. Starts with the tenant ID
-- 2. Follows by the API Endpoint
expectedPath :: String
expectedPath = unpack $ "/" <> T.tenantId <> "/oauth2/token"
