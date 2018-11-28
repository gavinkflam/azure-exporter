{-# LANGUAGE OverloadedStrings #-}

-- | Prometheus text sanitizing.
module Text.Prometheus.Sanitize
    (
      -- * Name
      sanitizeName
    , metricName
      -- * Label
    , sanitizeLabelValue
    ) where

import qualified Data.Text as T

-- | Sanitize metric and label name.
--
-- #. Retain only alphabetical characters and underscores.
--
-- #. Illegal characters will be replaced with underscore.
--
-- #. Leading and trailing underscore characters will be removed.
--
--   The metric names and label names will prepend a static prefix afterwards.
--   Thus we allow names starting with numeric characters.
--
--   <https://prometheus.io/docs/concepts/data_model/#metric-names-and-labels>
sanitizeName :: T.Text -> T.Text
sanitizeName =
    T.dropWhile (== '_')
    . T.dropWhileEnd (== '_')
    . T.map (\c -> if isLegalNameChar c then c else '_')

-- | Check if the character is a legal character for `sanitizeName`.
isLegalNameChar :: Char -> Bool
isLegalNameChar c
    | 'a' <= c && c <= 'z' = True
    | 'A' <= c && c <= 'Z' = True
    | '0' <= c && c <= '9' = True
    | c == '_'             = True
    | otherwise            = False

-- | Sanitize and standardize gauge name.
--
--   The metric name will be processed in three steps.
--
-- #. Sanitize by `sanitizeName` first.
--
-- #. Remove consecutive underscore characters.
--
-- #. Downcase the alphabetical characters.
metricName :: T.Text -> T.Text
metricName = T.toLower . removeConsecutiveUnderscore . sanitizeName
  where
    removeConsecutiveUnderscore =
        T.intercalate "_" . filter (/= T.empty) . T.splitOn "_"

-- | Sanitize label values.
--
--   Tags and addition info values often contains new line characters for unknown
--   reasons.
sanitizeLabelValue :: T.Text -> T.Text
sanitizeLabelValue = T.filter isLegalLabelValueChar

-- | Check if the character is a legal character for `sanitizeLabelValue`.
--
--   Line feed and carriage return charcters are considered illegal.
isLegalLabelValueChar :: Char -> Bool
isLegalLabelValueChar '\r' = False
isLegalLabelValueChar '\n' = False
isLegalLabelValueChar _    = True
