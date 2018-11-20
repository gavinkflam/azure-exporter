{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility functions related to Prometheus.
module Data.Prometheus
  -- * Data Model
  ( sanitizeName
  , metricName
  , sanitizeLabelValue
  ) where

import qualified Data.Text.Lazy as T

-- |
-- Sanitize metric and label name.
--
-- Retain only alphabetical characters and underscores.
-- Illegal characters will be replaced with underscore.
--
-- The metric names and label names will prepend a static prefix afterwards.
-- Thus we allow names starting with numeric characters.
--
-- <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
sanitizeName :: T.Text -> T.Text
sanitizeName = T.map (\c -> if isLegalNameChar c then c else '_')

-- | Check if the character is a legal character for `sanitizeName`.
isLegalNameChar :: Char -> Bool
isLegalNameChar c
  | 'a' <= c && c <= 'z' = True
  | 'A' <= c && c <= 'Z' = True
  | '0' <= c && c <= '9' = True
  | c == '_'             = True
  | otherwise            = False

-- |
-- Sanitize and standardize gauge name.
--
-- The metric name will be processed in three steps.
--
-- 1. Sanitized by `sanitizeName` first.
-- 2. Remove consecutive underscore characters.
-- 3. Downcase the alphabetical characters.
metricName :: T.Text -> T.Text
metricName =
  T.toLower . T.intercalate "_" . filter (/= T.empty) . T.splitOn "_" . sanitizeName

-- |
-- Sanitize label values.
--
-- Tags and addition info values often contains new line characters for unknown
-- reasons.
sanitizeLabelValue :: T.Text -> T.Text
sanitizeLabelValue = T.filter isLegalLabelValueChar

-- | Check if the character is a legal character for `sanitizeLabelValue`.
isLegalLabelValueChar :: Char -> Bool
isLegalLabelValueChar c
  | c == '\r' = False
  | c == '\n' = False
  | otherwise = True
