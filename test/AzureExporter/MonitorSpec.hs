-- |
-- Test the mechanism constructing `Gauge` from `ListMetricValuesResponse`.
module AzureExporter.MonitorSpec
  (
  -- * Spec
    spec
  ) where

import           AzureExporter.Monitor
import qualified Data.Dummy.Gauge as G
import           Expectations
import           Test.Hspec

-- | Spec for `Monitor`.
spec :: Spec
spec =
  describe "gauges" $
    it "derives the list of Gauge as expected" $
      gauges G.listMetricValuesResponse `shouldBe` [G.gauge]
