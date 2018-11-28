{-# LANGUAGE OverloadedStrings #-}

-- | Test authorization header adding mechanism for Azure RM API.
module Data.AzureRm.RequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import Data.AzureRm.Request (addAuthHeader)
import Network.HTTP.Client (Request, parseRequest_, requestHeaders)
import Network.HTTP.Types.Header
    (Header, RequestHeaders, hAuthorization, hContentType, hUserAgent)
import Test.Hspec

-- | Spec for `Request`.
spec :: Spec
spec = do
    let headers = requestHeaders $ addAuthHeader accessToken testRequest

    describe "addAuthHeader" $ do
        it "adds the expected authorization header" $
            headers `shouldContain` [authorizationHeader]
        it "preserves the original request headers" $
            headers `shouldContain` testHeaders

-- | Dummy request to test for auth header adding behaviour.
testRequest :: Request
testRequest =
    req { requestHeaders = testHeaders }
  where
    req = parseRequest_ "https://example.com"

-- | Request headers to test for headers preserving behaviour.
testHeaders :: RequestHeaders
testHeaders =
    [ (hContentType, "application/x-www-form-urlencoded")
    , (hUserAgent,   "azure-exporter")
    ]

-- | Authorization header to test for access token adding behaviour.
authorizationHeader :: Header
authorizationHeader = (hAuthorization, "Bearer " <> encodeUtf8 accessToken)

-- | Test access token for `addAuthHeader`.
accessToken :: Text
accessToken = "something-important"
