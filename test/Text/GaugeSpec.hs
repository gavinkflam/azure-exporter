-- |
-- Test the rendering of a `Gauge` in Prometheus exporter syntax.
module Text.GaugeSpec
  (
  -- * Spec
    spec
  ) where

import Test.Hspec

import qualified Data.Dummy.Gauge as G
import Expectations
import Text.Gauge

-- | Spec for `Gauge`.
spec :: Spec
spec =
  describe "renderGauge" $
    it "derives the Prometheus exporter syntax as expected" $
      renderGauge G.gauge `shouldBe` G.gaugeText
