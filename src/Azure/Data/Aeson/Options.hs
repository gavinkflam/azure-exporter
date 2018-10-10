-- |
-- Aeson encoding options.
module Azure.Data.Aeson.Options
  (
  -- * Options
    aesonOptions
  , oAuth2AesonOptions
  ) where

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
-- Aeson encoding options for OAuth2 API data structures.
--
-- The options include:
--
-- 1. Removing the underscore prefix from field names to derive the JSON keys.
-- 2. Converting the field names to lowercase snake case.
oAuth2AesonOptions :: Options
oAuth2AesonOptions =
  defaultOptions { fieldLabelModifier = quietSnake . dropWhile (== '_') }
