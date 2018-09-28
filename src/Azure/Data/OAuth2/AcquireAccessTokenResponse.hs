{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Azure.Data.OAuth2.AcquireAccessTokenResponse
  ( AcquireAccessTokenResponse (..)
  -- Lenses
  , accessToken
  , expiresIn
  , expiresOn
  , extExpiresIn
  , notBefore
  , resource
  , tokenType
  ) where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.Text.Lazy (Text)
import GHC.Generics
import Text.Casing (quietSnake)

data AcquireAccessTokenResponse =
  AcquireAccessTokenResponse { _accessToken  :: Text
                             , _expiresIn    :: Text
                             , _expiresOn    :: Text
                             , _extExpiresIn :: Text
                             , _notBefore    :: Text
                             , _resource     :: Text
                             , _tokenType    :: Text
                             } deriving (Generic, Show)

instance FromJSON AcquireAccessTokenResponse where
  parseJSON = genericParseJSON opts
    where opts = defaultOptions { fieldLabelModifier = quietSnake . drop 1 }

makeLenses ''AcquireAccessTokenResponse
