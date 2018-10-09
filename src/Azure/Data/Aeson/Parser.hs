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
import           Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text.Lazy as T

-- |
-- Extract readable error message from `ErrorResponse` or `ErrorValue`
-- JSON `ByteString`.
errorExtractor :: ByteString -> Maybe String
errorExtractor bs =
  head $ flip (++) [Nothing] $ map Just $ catMaybes results
    where results = [ errorResponseExtractor <$> A.decode bs
                    , errorValueExtractor <$> A.decode bs
                    ]

-- | Extract readable error message from `ErrorResponse`.
errorResponseExtractor :: E.ErrorResponse -> String
errorResponseExtractor e = errorValueExtractor $ e ^. E._error

-- | Extract readable error message from `ErrorValue`.
errorValueExtractor :: V.ErrorValue -> String
errorValueExtractor v = T.unpack $ v ^. V.code <> ": " <> v ^. V.message

-- |
-- Deserialize the JSON `ByteString`, or extract a readable error message using
-- the supplied error extractor when decoding fails.
--
-- If the error extractor fails, the original content of the response body
-- will be treated as the error message.
mapEitherDecode :: A.FromJSON a => (ByteString -> Maybe String) -> ByteString -> Either String a
mapEitherDecode f s = first g $ A.eitherDecode s
  where g = const $ fromMaybe (BS.unpack s) $ f s
