{-# LANGUAGE OverloadedStrings #-}

-- | Test access token deriving.
module Data.App.AccessTokenSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Test.Hspec

import qualified Data.App.AccessToken as Ak
import qualified Data.OAuth2.AcquireAccessTokenResponse as Akr

-- | Spec for `AccessToken`.
spec :: Spec
spec =
    describe "fromResponse" $
        it "derives the expected access token" $
            Ak.fromResponse testResponse `shouldBe` expectedAccessToken

-- | Test response for deriving access token.
testResponse :: Akr.AcquireAccessTokenResponse
testResponse = Akr.AcquireAccessTokenResponse
    { Akr.accessToken  = testTexts ! "accessToken"
    , Akr.expiresIn    = testTexts ! "expiresIn"
    , Akr.expiresOn    = testTexts ! "expiresOn"
    , Akr.extExpiresIn = testTexts ! "extExpiresIn"
    , Akr.notBefore    = testTexts ! "notBefore"
    , Akr.resource     = testTexts ! "resource"
    , Akr.tokenType    = testTexts ! "tokenType"
    }

-- | The expected access token derived.
expectedAccessToken :: Ak.AccessToken
expectedAccessToken = Ak.AccessToken
    { Ak.token     = testTexts ! "accessToken"
    , Ak.expiresOn = expiresOnTime
    }

-- | The expected expires on time.
expiresOnTime :: UTCTime
expiresOnTime =
    posixSecondsToUTCTime $ fromInteger $ read $ T.unpack $
        testTexts ! "expiresOn"

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("accessToken",  "top-secret")
    , ("expiresIn",    "3599")
    , ("expiresOn",    "1537935219")
    , ("extExpiresIn", "0")
    , ("notBefore",    "1537931319")
    , ("resource",     "https://management.azure.com/")
    , ("tokenType",    "Bearer")
    ]
