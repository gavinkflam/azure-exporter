{-# LANGUAGE TemplateHaskell #-}

module AzureExporter.Data.CSV
  (
  -- * Types
    CSV (..)
  , empty
  -- * Lenses
  , headers
  , rows
  -- * Utility
  , prependRow
  ) where

import           Data.Set (Set, fromList, union)
import qualified Data.Set as Set
import           Data.Text.Lazy (Text)

import           Control.Lens ((^.), makeLenses)
import           Data.HashMap.Strict (HashMap, keys)

-- | Data structure representing a CSV.
data CSV =
  CSV { _headers :: Set Text
      , _rows    :: [HashMap Text Text]
      } deriving (Eq, Show)

makeLenses ''CSV

-- | An empty CSV.
empty :: CSV
empty = CSV { _headers = Set.empty
            , _rows    = []
            }

-- | Prepend a row to the CSV while updating the headers automatically.
prependRow :: CSV -> HashMap Text Text -> CSV
prependRow csv row =
  CSV { _headers = (csv ^. headers) `union` fromList (keys row)
      , _rows    = row : (csv ^. rows)
      }
