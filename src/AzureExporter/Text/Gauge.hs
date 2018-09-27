{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AzureExporter.Text.Gauge
  ( renderGauge
  ) where

import qualified AzureExporter.Data.Gauge as G
import           Control.Lens ((^.))
import           Data.Monoid (mconcat)
import           Data.String.Here (iTrim)
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

renderGauge :: G.Gauge -> L.Text
renderGauge g = [iTrim|
# HELP ${L.fromStrict $ g ^. G.name} ${L.fromStrict $ g ^. G.help}
# TYPE ${L.fromStrict $ g ^. G.name} gauge
${L.fromStrict $ g ^. G.name}${renderLabels $ g ^. G.labels} ${g ^. G.value}
|]

renderLabels :: [(T.Text, T.Text)] -> L.Text
renderLabels [] = ""
renderLabels ls = L.intercalate "," $ map renderLabel ls

renderLabel :: (T.Text, T.Text) -> L.Text
renderLabel (name, value) =
  mconcat [L.fromStrict name, "=", L.fromStrict value]
