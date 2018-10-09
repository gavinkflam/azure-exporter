{-# LANGUAGE OverloadedStrings #-}

module Azure.Data.Aeson.Parser
  ( errorExtractor
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

errorExtractor :: E.ErrorResponse -> T.Text
errorExtractor e =
  mconcat [v ^. V.code , ": " , v ^. V.message] where v = e ^. E._error

mapEitherDecode :: (A.FromJSON a, A.FromJSON e) => (e -> T.Text) -> ByteString -> Either String a
mapEitherDecode f s = first g $ A.eitherDecode s
  where g = const $ maybe (BS.unpack s) (T.unpack . f) $ A.decode s
