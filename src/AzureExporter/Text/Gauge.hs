{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Utility to render a `Gauge` in Prometheus exporter syntax.
module AzureExporter.Text.Gauge
  (
  -- * Gauge
    renderGauge
  ) where

import qualified AzureExporter.Data.Gauge as G
import           Control.Lens ((^.))
import           Data.Monoid (mconcat)
import           Data.String.Here (iTrim)
import           Data.Text.Lazy (Text, intercalate)

-- | Render a `Gauge` in Prometheus exporter syntax.
--
-- @
-- # HELP metric_name Metric help message
-- # TYPE metric_name gauge
-- metric_name{name1="value1",name2="value2"} 0.42
-- @
renderGauge :: G.Gauge -> Text
renderGauge g = [iTrim|
# HELP ${g ^. G.name} ${g ^. G.help}
# TYPE ${g ^. G.name} gauge
${g ^. G.name}${renderLabels $ g ^. G.labels} ${g ^. G.value}
|]

-- |
-- Render labels in Prometheus exporter syntax.
--
-- @
-- {name1="value1",name2="value2"}
-- @
renderLabels :: [(Text, Text)] -> Text
renderLabels [] = ""
renderLabels ls = "{" <> intercalate "," (map renderLabel ls) <> "}"

-- | Render a label in Prometheus exporter syntax.
--
-- @
-- name="value"
-- @
renderLabel :: (Text, Text) -> Text
renderLabel (name, value) = mconcat [name, "=\"", value, "\""]
