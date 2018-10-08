{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.Error.ErrorResponse
  (
  -- * Types
    ErrorResponse (..)
  -- * Lenses
  , _error
  ) where

import Azure.Contract (aesonOptions)
import Azure.Data.Error.ErrorValue (ErrorValue)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- | Azure REST API ErrorResponse object.
--
-- <https://github.com/Microsoft/api-guidelines/blob/master/Guidelines.md#errorresponse--object>
newtype ErrorResponse =
  ErrorResponse { __error :: ErrorValue
                } deriving (Generic, Show)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON aesonOptions

makeLenses ''ErrorResponse
