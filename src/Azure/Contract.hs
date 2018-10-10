{-# LANGUAGE OverloadedStrings #-}

-- |
-- The constants for Azure API.
module Azure.Contract
  (
  -- * Versions
    monitorApiVersion
  ) where

import Data.Text.Lazy (Text)

-- | The API version for Azure monitor.
monitorApiVersion :: Text
monitorApiVersion = "2018-01-01"
