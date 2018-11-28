{-# LANGUAGE OverloadedStrings #-}

-- | Test Prometheus sanitizing utility functions.
module Text.Prometheus.SanitizeSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text)

import Test.Hspec
import qualified Text.Prometheus.Sanitize as Sn

-- | Spec for `Sanitize`.
spec :: Spec
spec = do
    describe "sanitizeName" $ do
        it "sanitizes the name text" $
            Sn.sanitizeName testName `shouldBe` expectedName
        it "is idempotent" $
            Sn.sanitizeName (Sn.sanitizeName testName) `shouldBe` expectedName

    describe "metricName" $ do
        it "sanitizes and standardizes the metric name text" $
            Sn.metricName testName `shouldBe` expectedMetricName
        it "is idempotent" $
            Sn.metricName (Sn.metricName testName) `shouldBe` expectedMetricName

    describe "sanitizeLabelValue" $ do
        it "sanitizes and standardizes the label value text" $
            Sn.sanitizeLabelValue testLabelValue `shouldBe` expectedLabelValue
        it "is idempotent" $
            Sn.sanitizeLabelValue (Sn.sanitizeLabelValue testLabelValue)
            `shouldBe` expectedLabelValue

-- | Name to test for sanitization.
testName :: Text
testName = "A/quick_-_brown=_Fox___jumps__0oVer the*_lazy3dog.."

-- | Label value to test for sanitization.
testLabelValue :: Text
testLabelValue = "life @#is-but__a\ndream\r\n"

-- | Expected name sanitized using `sanitizeName`.
--
--   Illegal characters should be removed.
expectedName :: Text
expectedName = "A_quick___brown__Fox___jumps__0oVer_the__lazy3dog__"

-- | Expected metric name sanitized and standardized using `metricName`.
--
-- #. Illegal characters should be removed.
--
-- #. Consecurive underscore should be removed.
--
-- #. Alphabetical characters should be downcased.
expectedMetricName :: Text
expectedMetricName = "a_quick_brown_fox_jumps_0over_the_lazy3dog_"

-- | Expected label value sanitized using `sanitizeLabelValue`.
expectedLabelValue :: Text
expectedLabelValue = "life @#is-but__adream"
