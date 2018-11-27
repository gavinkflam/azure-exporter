-- | Test the encoding of `Gauge`s.
module Data.Prometheus.GaugeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Csv.IncrementMod (encodeNamedRecords)
import Data.Csv.ToHeader (toHeader)
import Test.Hspec

import qualified Data.Prometheus.TestData as D

-- | Spec for `Gauge`.
spec :: Spec
spec = do
    describe "csv header" $
        it "derive csv header from gauges" $
            toHeader D.testGauges `shouldBe` D.expectedHeader
    describe "csv text" $
        it "derive csv text from gauges" $
            encodeNamedRecords D.testGauges `shouldBe` D.expectedCsv
