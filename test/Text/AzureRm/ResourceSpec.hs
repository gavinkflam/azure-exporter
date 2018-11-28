{-# LANGUAGE OverloadedStrings #-}

-- | Test utility functions related to Azure resources.
module Text.AzureRm.ResourceSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text, toLower)
import qualified Data.Text as T

import Control.Lens ((^.))
import qualified Data.AzureRm.ResourceMetadata as M
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import Test.Hspec
import qualified Text.AzureRm.Resource as R

-- | Spec for `Resource`.
spec :: Spec
spec = do
    let meta = R.parseResourceId resourceId
        id'  = R.resourceId subResourceId

    describe "parseResourceId" $ do
        it "extracts the resource group correctly" $
            (meta ^. M.resourceGroup)
            `shouldBe` toLower (testDatastore ! "resourceGroup")
        it "extracts the resource name correctly" $
            (meta ^. M.resourceName)
            `shouldBe` toLower (testDatastore ! "resourceName")
        it "extracts the resource provider correctly" $
            (meta ^. M.resourceProvider)
            `shouldBe` toLower (testDatastore ! "resourceProvider")
        it "extracts the resource type correctly" $
            (meta ^. M.resourceType)
            `shouldBe` toLower (testDatastore ! "resourceType")
        it "extracts the subscription ID correctly" $
            (meta ^. M.subscriptionId)
            `shouldBe` toLower (testDatastore ! "subscriptionId")

    describe "resourceId" $ do
        it "extracts the top-level resource ID" $
            id' `shouldBe` toLower resourceId
        it "is idempotent" $
            id' `shouldBe` R.resourceId id'

-- | Test resource Id.
resourceId :: Text
resourceId = T.concat
    [ "/subscriptions/"
    , testDatastore ! "subscriptionId"
    , "/resourceGroups/"
    , testDatastore ! "resourceGroup"
    , "/providers/"
    , testDatastore ! "resourceProvider"
    , "/virtualMachines/"
    , testDatastore ! "resourceName"
    ]

-- | Test sub-resource Id.
subResourceId :: Text
subResourceId = resourceId <> "/extensions/LinuxDiagnostic"

-- | Datastore for test data.
testDatastore :: HashMap Text Text
testDatastore = HM.fromList
    [ ("resourceGroup",    "DummyGroup")
    , ("resourceName",     "DummyVM")
    , ("resourceType",     "virtualMachines")
    , ("resourceProvider", "Microsoft.Compute")
    , ("subscriptionId",   "312a4ad3-78e8-4b85-aa85-fdf7041f8155")
    ]
