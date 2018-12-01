{-# LANGUAGE OverloadedStrings #-}

-- | Test deriving csv from records with variable header for each record.
module Data.Csv.IncrementModSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS

import Data.Csv.IncrementMod (encodeNamedRecords)
import Test.Hspec

import qualified Data.Csv.TestData as D

-- | Spec for `IncrementMod`.
spec :: Spec
spec =
    describe "encodeNamedRecords" $
        it "derives csv text from variable header records" $
            encodeNamedRecords D.testRecords `shouldBe` expectedCsv

-- | Expected csv text derived from `testRecords`.
--
--   Some fields should be blank because of the variable header.
expectedCsv :: LBS.ByteString
expectedCsv = LBS.intercalate "\r\n"
    [ "a,b,c,d,e,f,x,y"
    , "1,,3,,,,2,"
    , ",4,,5,6,,,"
    , "7,,,,8,10,,9"
    , "18,17,16,15,14,13,12,11"
    , ""
    ]
