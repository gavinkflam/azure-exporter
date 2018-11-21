{-# LANGUAGE OverloadedStrings #-}

-- | The constants for Azure API.
module Data.Contract
    (
      -- * Versions
      monitorApiVersion
    , billingApiVersion
    ) where

import Data.Text.Lazy (Text)

-- | The API version for Azure monitor.
monitorApiVersion :: Text
monitorApiVersion = "2018-01-01"

-- | The API version for Azure billing.
billingApiVersion :: Text
billingApiVersion = "2015-06-01-preview"
