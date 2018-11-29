-- | Test the encoding of `Gauge`s.
module Data.Prometheus.GaugeSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString.Builder (toLazyByteString)

import Data.Csv.IncrementMod (encodeNamedRecords)
import Data.Csv.ToHeader (toHeader)
import Data.Prometheus.Gauge (renderGauges)
import Test.Hspec

import qualified Data.Prometheus.TestData as D

-- | Spec for `Gauge`.
spec :: Spec
spec = do
    describe "toHeader" $
        it "derive csv header from gauges" $
            toHeader D.testGauges `shouldBe` D.expectedHeader
    describe "encodeNamedRecords" $
        it "derive csv text from gauges" $
            encodeNamedRecords D.testGauges `shouldBe` D.expectedCsv
    describe "renderGauges" $
        it "derive exposition text from gauges" $
            toLazyByteString (renderGauges D.simpleTestGauges)
            `shouldBe` D.expectedSimpleTestGaugesExpositionText
