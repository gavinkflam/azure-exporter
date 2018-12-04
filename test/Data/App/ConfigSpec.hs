{-# LANGUAGE OverloadedStrings #-}

-- | Test reading configuration from environment variables.
module Data.App.ConfigSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.Text as T

import Control.Exception (evaluate)
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Test.Hspec

import Control.Monad.System.MonadEnv (runPure)
import qualified Data.App.Config as Cf

-- | Spec for `Config`.
spec :: Spec
spec =
    describe "getConfig" $ do
        it "gets the expected config" $
            runPure testVars1 Cf.getConfig `shouldBe` expectedConfig1
        it "gets the expected config with missing optional variables" $
            runPure testVars2 Cf.getConfig `shouldBe` expectedConfig2
        it "throws error without client_id variable" $
            testGetConfigWithoutVar "CLIENT_ID"
        it "throws error without client_secret variable" $
            testGetConfigWithoutVar "CLIENT_SECRET"
        it "throws error without subscription_id variable" $
            testGetConfigWithoutVar "SUBSCRIPTION_ID"
        it "throws error without tenant_id variable" $
            testGetConfigWithoutVar "TENANT_ID"

-- | Test `getConfig` without the variable of key `k` if it throws the
--   expected error.
testGetConfigWithoutVar :: String -> Expectation
testGetConfigWithoutVar k =
    evaluate (runPure var Cf.getConfig) `shouldThrow` err
  where
    var = HM.delete k testVars1
    err = errorCall $ k <> ": getEnv: does not exist"

-- | Mock environment variables.
testVars1 :: HashMap String String
testVars1 = HM.fromList
    [ ("CLIENT_ID",       testTexts ! "CLIENT_ID")
    , ("CLIENT_SECRET",   testTexts ! "CLIENT_SECRET")
    , ("PORT",            testTexts ! "PORT")
    , ("SUBSCRIPTION_ID", testTexts ! "SUBSCRIPTION_ID")
    , ("TENANT_ID",       testTexts ! "TENANT_ID")
    , ("OFFER_ID",        testTexts ! "OFFER_ID")
    , ("CURRENCY",        testTexts ! "CURRENCY")
    , ("LOCALE",          testTexts ! "LOCALE")
    , ("REGION_INFO",     testTexts ! "REGION_INFO")
    ]

-- | Mock environment variables with optional config items missing.
testVars2 :: HashMap String String
testVars2 = HM.fromList
    [ ("CLIENT_ID",       testTexts ! "CLIENT_ID")
    , ("CLIENT_SECRET",   testTexts ! "CLIENT_SECRET")
    , ("SUBSCRIPTION_ID", testTexts ! "SUBSCRIPTION_ID")
    , ("TENANT_ID",       testTexts ! "TENANT_ID")
    ]

-- | Expected config for `testVars1`.
expectedConfig1 :: Cf.Config
expectedConfig1 = Cf.Config
    { Cf._clientId       = T.pack $ testTexts ! "CLIENT_ID"
    , Cf._clientSecret   = T.pack $testTexts ! "CLIENT_SECRET"
    , Cf._port           = read $ testTexts ! "PORT"
    , Cf._subscriptionId = T.pack $ testTexts ! "SUBSCRIPTION_ID"
    , Cf._tenantId       = T.pack $ testTexts ! "TENANT_ID"
    , Cf._offerId        = T.pack $ testTexts ! "OFFER_ID"
    , Cf._currency       = T.pack $ testTexts ! "CURRENCY"
    , Cf._locale         = T.pack $ testTexts ! "LOCALE"
    , Cf._regionInfo     = T.pack $ testTexts ! "REGION_INFO"
    }

-- | Expected config for `testVars2`.
expectedConfig2 :: Cf.Config
expectedConfig2 = Cf.Config
    { Cf._clientId       = T.pack $ testTexts ! "CLIENT_ID"
    , Cf._clientSecret   = T.pack $testTexts ! "CLIENT_SECRET"
    , Cf._port           = 9492
    , Cf._subscriptionId = T.pack $ testTexts ! "SUBSCRIPTION_ID"
    , Cf._tenantId       = T.pack $ testTexts ! "TENANT_ID"
    , Cf._offerId        = "MS-AZR-0003p"
    , Cf._currency       = "USD"
    , Cf._locale         = "en-US"
    , Cf._regionInfo     = "US"
    }

-- | Texts for test data.
testTexts :: HashMap String String
testTexts = HM.fromList
    [ ("CLIENT_ID",       "blah-blah-blah")
    , ("CLIENT_SECRET",   "secret")
    , ("PORT",            "8080")
    , ("SUBSCRIPTION_ID", "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    , ("TENANT_ID",       "82a54265-2a0e-438b-84fa-c04f029f1077")
    , ("OFFER_ID",        "0044P")
    , ("CURRENCY",        "XTS")
    , ("LOCALE",          "en-UK")
    , ("REGION_INFO",     "UK")
    ]
