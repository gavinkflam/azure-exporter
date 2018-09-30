{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.OAuth2.ErrorResponse
  ( ErrorResponse (..)
  -- Lenses
  , correlationId
  , _error
  , errorCodes
  , errorDescription
  , timestamp
  , traceId
  ) where

import Azure.Contract (oAuth2AesonOptions)
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics

-- ErrorResponse
data ErrorResponse =
  ErrorResponse { _correlationId    :: Text
                , __error           :: Text
                , _errorCodes       :: [Int]
                , _errorDescription :: Text
                , _timestamp        :: Text
                , _traceId          :: Text
                } deriving (Generic, Show)

instance FromJSON ErrorResponse where
  parseJSON = genericParseJSON oAuth2AesonOptions

makeLenses ''ErrorResponse
