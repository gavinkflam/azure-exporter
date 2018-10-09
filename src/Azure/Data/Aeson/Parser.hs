{-# LANGUAGE OverloadedStrings #-}

-- |
-- JSON decoding and error extraction using Aeson.
module Azure.Data.Aeson.Parser
  (
  -- * Error
    errorExtractor
  -- * Decode
  , mapEitherDecode
  ) where

import           Azure.Data.Error.ErrorResponse as E
import           Azure.Data.Error.ErrorValue as V
import           Control.Lens ((^.))
import qualified Data.Aeson as A
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T

-- | Extractor readable error message from `ErrorResponse`.
errorExtractor :: E.ErrorResponse -> String
errorExtractor e =
  T.unpack $ v ^. V.code <> ": " <> v ^. V.message where v = e ^. E._error

-- |
-- Deserialize the JSON `ByteString`, or extract a readable error message using
-- the supplied error extractor when decoding fails.
mapEitherDecode :: (A.FromJSON a, A.FromJSON e) => (e -> String) -> ByteString -> Either String a
mapEitherDecode f s = first g $ A.eitherDecode s
  where g = const $ maybe (BS.unpack s) f $ A.decode s
