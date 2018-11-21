{-# LANGUAGE OverloadedStrings #-}

-- | JSON decoding and error extraction using Aeson.
module Data.Response.Aeson
    (
      -- * Error
      ErrorHandler
    , errorExtractor
      -- * Decode
    , mapEitherDecode
    ) where

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text.Lazy as T

import Control.Lens ((^.))
import qualified Data.Aeson as A
import Network.HTTP.Client (Response, responseBody, responseStatus)
import Network.HTTP.Types (ok200, created201)

import qualified Data.Error.ErrorResponse as E
import qualified Data.Error.ErrorValue as V

-- | Error handler function type.
type ErrorHandler = ByteString -> Maybe String

-- | Extract readable error message from `ErrorResponse` or `ErrorValue`
--   JSON `ByteString`.
errorExtractor :: ErrorHandler
errorExtractor bs =
    head $ flip (++) [Nothing] $ map Just $ catMaybes results
  where
    results =
        [ errorResponseExtractor <$> A.decode bs
        , errorValueExtractor <$> A.decode bs
        ]

-- | Extract readable error message from `ErrorResponse`.
errorResponseExtractor :: E.ErrorResponse -> String
errorResponseExtractor e = errorValueExtractor (e ^. E._error)

-- | Extract readable error message from `ErrorValue`.
errorValueExtractor :: V.ErrorValue -> String
errorValueExtractor v = T.unpack $ (v ^. V.code) <> ": " <> (v ^. V.message)

-- | On success responses (200 or 201), deserialize the JSON response body or
--   return the deserialization error message.
--
--   On failed responses, extract a readable error message using the supplied
--   error extractor. If the error extractor fails, the original content of the
--   response body will be treated as the error message.
mapEitherDecode
    :: A.FromJSON a => ErrorHandler -> Response ByteString -> Either String a
mapEitherDecode f res =
    case responseStatus res of
        s | s `elem` [ok200, created201] -> A.eitherDecode b
        _                                -> Left $ fromMaybe (BS.unpack b) $ f b
      where
        b = responseBody res
