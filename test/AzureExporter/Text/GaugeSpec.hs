-- |
-- Test the rendering of a `Gauge` in Prometheus exporter syntax.
module AzureExporter.Text.GaugeSpec
  (
  -- * Spec
    spec
  ) where

import           AzureExporter.Text.Gauge
import qualified Data.Dummy.Gauge as G
import           Expectations
import           Test.Hspec

-- | Spec for `Gauge`.
spec :: Spec
spec =
  describe "renderGauge" $
    it "derives the Prometheus exporter syntax as expected" $
      renderGauge G.gauge `shouldBe` G.gaugeText