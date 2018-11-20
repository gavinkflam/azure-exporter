-- |
-- Test the mechanism constructing `Gauge` from `ListMetricValuesResponse`.
module Data.MonitorSpec
  (
  -- * Spec
    spec
  ) where

import           Data.Monitor
import qualified Data.Dummy.Gauge as G
import           Expectations
import           Test.Hspec

-- | Spec for `Monitor`.
spec :: Spec
spec =
  describe "gauges" $
    it "derives the list of Gauge as expected" $
      gauges G.listMetricValuesResponse `shouldBe` [G.gauge]
