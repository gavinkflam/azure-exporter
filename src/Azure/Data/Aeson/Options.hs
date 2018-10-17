-- |
-- Aeson encoding options.
module Azure.Data.Aeson.Options
  (
  -- * Options
    aesonOptions
  , capitalizeAesonOptions
  , oAuth2AesonOptions
  ) where

import Data.Char (toUpper)

import Data.Aeson
import Text.Casing (quietSnake)

-- |
-- Aeson encoding options for general API data structures.
--
-- The options include:
--
-- 1. Removing the underscore prefix from field names to derive the JSON keys.
aesonOptions :: Options
aesonOptions =
  defaultOptions { fieldLabelModifier = dropWhile (== '_') }

-- |
-- Aeson encoding options for API data structures with capitalized keys.
--
-- The options include:
--
-- 1. Removing the underscore prefix from field names.
-- 2. Capitalize the first character.
capitalizeAesonOptions :: Options
capitalizeAesonOptions =
  defaultOptions { fieldLabelModifier = capitalize . dropWhile (== '_') }

-- | Capitalize the first character of a given `String`.
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = [toUpper x] ++ xs

-- |
-- Aeson encoding options for OAuth2 API data structures.
--
-- The options include:
--
-- 1. Removing the underscore prefix from field names to derive the JSON keys.
-- 2. Converting the field names to lowercase snake case.
oAuth2AesonOptions :: Options
oAuth2AesonOptions =
  defaultOptions { fieldLabelModifier = quietSnake . dropWhile (== '_') }
