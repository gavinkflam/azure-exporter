-- | Test error capturing and value extraction with `MonadFail`.
module Control.Monad.Fail.TransSpec
    (
      -- * Spec
      spec
    ) where

import Test.Hspec

import Control.Monad.Fail.Trans (failLeft, failNothing)

-- | Spec for `Trans`.
spec :: Spec
spec = do
    describe "failLeft" $ do
        it "fails with the left message" $
            failLeft (Left "left") `shouldThrow` (== userError "left")
        it "extracts the right value" $
            failLeft (Right "right") `shouldReturn` "right"
    describe "failNothing" $ do
        it "fails nothing with the given message" $
            failNothing "error" Nothing `shouldThrow` (== userError "error")
        it "extracts the right value" $
            failNothing "error" (Just "something") `shouldReturn` "something"
