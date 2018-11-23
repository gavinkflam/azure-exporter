-- | Test the encoding of `Gauge`s.
module Data.GaugeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Csv.Incremental (encodeByName, encodeNamedRecord)
import Test.Hspec

import Data.Csv.HasHeaders (uniqueHeaders)
import qualified Data.Dummy.Gauge as G

-- | Spec for `Gauge`.
spec :: Spec
spec =
    describe "encode as CSV" $
        it "encode in CSV as expected" $
            csv `shouldBe` G.gaugesCsv
          where
            gs  = [G.usageGauge, G.costGauge]
            hs  = uniqueHeaders gs
            csv = toStrict $ decodeUtf8 $ encodeByName hs $ f gs
            f   = foldr ((<>) . encodeNamedRecord) mempty
