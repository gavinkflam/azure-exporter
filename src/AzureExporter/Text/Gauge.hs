{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module AzureExporter.Text.Gauge
  ( renderGauge
  ) where

import qualified AzureExporter.Data.Gauge as G
import           Control.Lens ((^.))
import           Data.Monoid (mconcat)
import           Data.String.Here (iTrim)
import           Data.Text.Lazy (Text, intercalate)

renderGauge :: G.Gauge -> Text
renderGauge g = [iTrim|
# HELP ${g ^. G.name} ${g ^. G.help}
# TYPE ${g ^. G.name} gauge
${g ^. G.name}${renderLabels $ g ^. G.labels} ${g ^. G.value}
|]

renderLabels :: [(Text, Text)] -> Text
renderLabels [] = ""
renderLabels ls =
  mconcat [ "{"
          , intercalate "," $ map renderLabel ls
          , "}"
          ]

renderLabel :: (Text, Text) -> Text
renderLabel (name, value) = mconcat [name, "=", value]
