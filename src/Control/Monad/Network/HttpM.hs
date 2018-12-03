{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

-- | HTTP request monad.
module Control.Monad.Network.HttpM
    (
      -- * Monad
      HttpM (..)
      -- * Free Monad Interpreter
    , runPure
      -- * Json
    , httpJson
    ) where

import qualified Data.ByteString.Lazy as LBS

import Control.Monad.Free (Free(..), liftF)
import Data.Aeson (FromJSON)
import Network.HTTP.Client (Manager, Request, Response)
import qualified Network.HTTP.Client as Ht

import Data.Response.Aeson (ErrorHandler, mapEitherDecode)

-- | HTTP request monad.
class Monad m => HttpM m where
    httpLbs :: Request -> Manager -> m (Response LBS.ByteString)

-- | Implement `HttpM` as `IO`.
instance HttpM IO where
    httpLbs = Ht.httpLbs

-- | HTTP request operations represented as functors.
data FreeHttpMF x
    = HttpLbs Request Manager (Response LBS.ByteString -> x)
    deriving Functor

-- | HTTP request oprations contained as free monad.
type FreeHttpM = Free FreeHttpMF

-- | Translate `HttpM` to `FreeHttpM` free monad.
instance HttpM FreeHttpM where
    httpLbs r m = liftF (HttpLbs r m id)

-- | Run `FreeHttpM` sequence with mocked response.
runPure :: Response LBS.ByteString -> FreeHttpM a -> a
runPure _ (Pure x)                 = x
runPure res (Free (HttpLbs _ _ f)) = runPure res $ f res

-- | Run HTTP request and parse the response into error or `FromJson` data.
httpJson
    :: (FromJSON a, HttpM m)
    => ErrorHandler -> Manager -> Request -> m (Either String a)
httpJson handler manager request =
    mapEitherDecode handler <$> httpLbs request manager
