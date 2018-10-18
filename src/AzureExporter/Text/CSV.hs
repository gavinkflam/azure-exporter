{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility to render a `CSV`.
module AzureExporter.Text.CSV
  (
  -- * CSV
    renderCSV
  ) where

import           Data.Text.Lazy (Text, intercalate, pack, unlines)

import           Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import qualified Data.Set as Set
import           Prelude hiding (unlines)

import qualified AzureExporter.Data.CSV as C
import qualified AzureExporter.Data.Gauge as G

-- | Render a `CSV` in `Text`.
renderCSV :: C.CSV -> Text
renderCSV csv =
  unlines $ intercalate "," headers : map (renderRow headers) (csv ^. C.rows)
    where headers = Set.toList (csv ^. C.headers)

-- | Render a `CSV` row in `Text`.
renderRow :: [Text] -> H.HashMap Text Text -> Text
renderRow headers row =
  intercalate "," $ map (flip (H.lookupDefault "") row) headers
