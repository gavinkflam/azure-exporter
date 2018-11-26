-- | Test the encoding of `Gauge`s.
module Data.GaugeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Csv.IncrementMod (encodeNamedRecords)
import Test.Hspec

import qualified Data.Dummy.Gauge as G

-- | Spec for `Gauge`.
spec :: Spec
spec =
    describe "encode as CSV" $
        it "encode in CSV as expected" $
            csv `shouldBe` G.gaugesCsv
          where
            gs  = [G.usageGauge, G.costGauge]
            csv = toStrict $ decodeUtf8 $ encodeNamedRecords gs
