-- |
-- Test utility functions related to Azure resources.
module Data.ResourceSpec
  (
  -- * Spec
    spec
  ) where

import           Data.Resource
import qualified Data.ResourceMetadata as M
import           Control.Lens ((^.))
import qualified Data.Dummy.Text as T
import           Data.Text.Lazy (toLower)
import           Expectations
import           Test.Hspec
import           Util.Text (toBS)

-- | Spec for `Resource`.
spec :: Spec
spec = do
  let meta = parseResourceId T.resourceId
      id   = resourceId T.subResourceId

  describe "parseResourceId" $ do
    it "extracts the resource group correctly" $
      (meta ^. M.resourceGroup) `shouldBe` toLower T.resourceGroup

    it "extracts the resource name correctly" $
      (meta ^. M.resourceName) `shouldBe` toLower T.resourceName

    it "extracts the resource provider correctly" $
      (meta ^. M.resourceProvider) `shouldBe` toLower T.resourceProvider

    it "extracts the resource type correctly" $
      (meta ^. M.resourceType) `shouldBe` toLower T.resourceType

    it "extracts the subscription ID correctly" $
      (meta ^. M.subscriptionId) `shouldBe` toLower T.subscriptionId

  describe "resourceId" $ do
    it "extracts the top-level resource ID" $
      id `shouldBe` toLower T.resourceId

    it "is idempotent" $
      id `shouldBe` resourceId id