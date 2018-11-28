{-# LANGUAGE OverloadedStrings #-}

-- | Test utility functions related to the HTTP Client .
module Data.AzureRm.RequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text.Encoding (encodeUtf8)

import qualified Network.HTTP.Client as C
import qualified Network.HTTP.Types.Header as H
import Test.Hspec

import Data.AzureRm.Request
import qualified Data.Dummy.Text as T

-- | Spec for `Timespan`.
spec :: Spec
spec = do
    let headers = C.requestHeaders $ addAuthHeader T.accessToken dummyRequest

    describe "addAuthHeader" $ do
        it "contains the newly added auth header" $
            headers `shouldContain` [authHeader]

        it "contains the original request headers" $
            headers `shouldContain` dummyHeaders

-- | Dummy request to test for auth header adding behaviour.
dummyRequest :: C.Request
dummyRequest =
    req { C.requestHeaders = dummyHeaders }
  where
    req = C.parseRequest_ "https://example.com"

-- | Dummy request headers to test for headers perserving behaviour.
dummyHeaders :: H.RequestHeaders
dummyHeaders =
    [ (H.hContentType, "application/x-www-form-urlencoded")
    , (H.hUserAgent, "Data.AzureRm.RequestSpec")
    ]

-- | Dummy auth header constructed from `accessToken` dummy text.
authHeader :: H.Header
authHeader = (H.hAuthorization, encodeUtf8 $ "Bearer " <> T.accessToken)
