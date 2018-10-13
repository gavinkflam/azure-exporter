{-# LANGUAGE OverloadedStrings #-}

module Data.Dummy.Gauge
  (
  -- * Gauge
    gauge
  , gaugeText
  ) where

import qualified AzureExporter.Data.Gauge as G
import           Control.Lens ((^.))
import qualified Data.Dummy.Text as T
import           Data.Text.Lazy (Text, intercalate, pack, toLower)

-- | Dummy `Gauge`
gauge :: G.Gauge
gauge =
  G.Gauge { G._name   = "azure_virtualmachines_cpu_percentage_average"
          , G._help   = "azure_virtualmachines_cpu_percentage_average"
          , G._labels = [ ("resource_group",    toLower T.resourceGroup)
                        , ("resource_id",       toLower T.resourceId)
                        , ("resource_name",     toLower T.resourceName)
                        , ("resource_provider", toLower T.resourceProvider)
                        , ("resource_region",   toLower T.resourceRegion)
                        , ("resource_type",     toLower T.resourceType)
                        , ("subResource_id",    T.subscriptionId)
                        ]
          , G._value  = 4.2
          }

-- | `gauge` in Prometheus exporter syntax
gaugeText :: Text
gaugeText =
  "# HELP " <> gauge ^. G.name <> " " <> gauge ^. G.help <> "\n" <>
  "# TYPE " <> gauge ^. G.name <> " gauge\n" <>
  gauge ^. G.name <> "{" <>
    intercalate "," (map label $ gauge ^. G.labels) <>
  "} " <> pack (show $ gauge ^. G.value)
    where label (k, v) = k <> "=\"" <> v <> "\""
