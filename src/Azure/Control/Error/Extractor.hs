{-# LANGUAGE OverloadedStrings #-}

module Azure.Control.Error.Extractor
  ( eitherDecode
  , errorExtractor
  ) where

import           Azure.Data.Error.ErrorResponse as E
import           Azure.Data.Error.ErrorValue as V
import           Control.Lens ((^.))
import qualified Data.Aeson as A
import           Data.Bifunctor (first)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Text.Lazy as T

eitherDecode :: A.FromJSON a => ByteString -> Either String a
eitherDecode s = first (errorExtractor s) $ A.eitherDecode s

errorExtractor :: ByteString -> (String -> String)
errorExtractor s =
  const $ maybe (BS.unpack s) (T.unpack . readableErrorMessage) $ A.decode s

readableErrorMessage :: E.ErrorResponse -> T.Text
readableErrorMessage e =
  mconcat [value ^. V.code , ": " , value ^. V.message]
    where value = e ^. E._error
