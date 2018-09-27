{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Data.Gauge
  ( Gauge
  -- Lenses
  , name
  , help
  , labels
  , value
  ) where

import Control.Lens (makeLenses)
import Data.Scientific (Scientific)
import Data.Text (Text)

data Gauge =
  Gauge { _name   :: Text
        , _help   :: Text
        , _labels :: [(Text, Text)]
        , _value  :: Scientific
        }

makeLenses ''Gauge
