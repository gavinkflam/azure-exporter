-- |
-- Test the mechanism constructing `Gauge` from `ListMetricValuesResponse`.
module Data.MonitorSpec
  (
  -- * Spec
    spec
  ) where

import Test.Hspec

import qualified Data.Dummy.Gauge as G
import Data.Monitor
import Expectations

-- | Spec for `Monitor`.
spec :: Spec
spec =
  describe "gauges" $
    it "derives the list of Gauge as expected" $
      gauges G.listMetricValuesResponse `shouldBe` [G.gauge]
