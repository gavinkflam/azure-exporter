{-# LANGUAGE OverloadedStrings #-}

-- |
-- Utility to render a `CSV`.
module Text.CSV
  (
  -- * CSV
    renderCSV
  ) where

import           Data.Text.Lazy (Text, intercalate, pack, unlines)

import           Control.Lens ((^.))
import qualified Data.HashMap.Strict as H
import           Data.List (sort)
import           Data.Set (Set, (\\), fromList, toList)
import           Prelude hiding (unlines)

import qualified AzureExporter.Data.CSV as C
import qualified AzureExporter.Data.Gauge as G

-- | Render a `CSV` in `Text`.
renderCSV :: C.CSV -> Text
renderCSV csv =
  unlines $ intercalate "," headers : map (renderRow headers) (csv ^. C.rows)
    where headers = sortHeaders (csv ^. C.headers)

-- | Render a `CSV` row in `Text`.
renderRow :: [Text] -> H.HashMap Text Text -> Text
renderRow headers row =
  intercalate "," $ map (flip (H.lookupDefault "") row) headers

-- |
-- Sort headers in strict ordering.
--
-- series should go first, followed by value, timestamp and labels.
sortHeaders :: Set Text -> [Text]
sortHeaders hs =
  coreHeaders ++ labelHeaders
  where coreHeaders  = ["series", "value", "timestamp"]
        labelHeaders = sort $ toList $ hs \\ fromList coreHeaders
