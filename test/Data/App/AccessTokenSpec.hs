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
    { Akr._accessToken  = testTexts ! "accessToken"
    , Akr._expiresIn    = testTexts ! "expiresIn"
    , Akr._expiresOn    = testTexts ! "expiresOn"
    , Akr._extExpiresIn = testTexts ! "extExpiresIn"
    , Akr._notBefore    = testTexts ! "notBefore"
    , Akr._resource     = testTexts ! "resource"
    , Akr._tokenType    = testTexts ! "tokenType"
    }

-- | The expected access token derived.
expectedAccessToken :: Ak.AccessToken
expectedAccessToken = Ak.AccessToken
    { Ak._token     = testTexts ! "accessToken"
    , Ak._expiresOn = expiresOnTime
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
