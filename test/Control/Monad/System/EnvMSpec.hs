{-# LANGUAGE OverloadedStrings #-}

-- | Test environment variables reading monad.
module Control.Monad.System.EnvMSpec
    (
      -- * Spec
      spec
    ) where

import Data.Maybe (fromMaybe)

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Test.Hspec

import Control.Monad.System.EnvM (EnvM, getEnv, lookupEnv, runPure)

-- | Spec for `EnvM`.
spec :: Spec
spec =
    describe "runPure" $
        it "interprets the operations and return the expected text" $
            runPure testVars testSeq `shouldBe` expectedText

-- | Test sequence to derive a text from environment variables.
testSeq :: EnvM m => m String
testSeq = do
    verb    <- getEnv "VERB"
    subject <- fromMaybe "nobody" <$> lookupEnv "SUBJECT"
    symbol  <- fromMaybe "." <$> lookupEnv "SYMBOL"

    return $ concat [verb, " ", subject, symbol]

-- | Test variables to mock environment variables.
testVars :: HashMap String String
testVars = HM.fromList
    [ ("VERB",    "hello")
    , ("SUBJECT", "world")
    ]

-- | Expected text for running `testSeq`.
expectedText :: String
expectedText = "hello world."
