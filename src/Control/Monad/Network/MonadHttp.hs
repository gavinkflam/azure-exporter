{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

-- | HTTP request monad.
module Control.Monad.Network.MonadHttp
    (
      -- * Monad
      MonadHttp (..)
      -- * Free Monad Interpreter
    , runPure
      -- * Json
    , httpJson
    ) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS

import Control.Monad.Free (Free(..), liftF)
import Data.Aeson (FromJSON)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Manager, Request, Response)
import qualified Network.HTTP.Client as Ht

import Data.Response.Aeson (ErrorHandler, mapEitherDecode)

-- | HTTP request monad.
class Monad m => MonadHttp m where
    httpLbs :: Request -> Manager -> m (Response LBS.ByteString)

-- | Implement `MonadHttp` as `IO`.
instance MonadHttp IO where
    httpLbs = Ht.httpLbs

-- | HTTP request operations represented as functors.
data FreeHttpF x
    = HttpLbs Request Manager (Response LBS.ByteString -> x)
    deriving Functor

-- | HTTP request oprations contained as free monad.
type FreeHttp = Free FreeHttpF

-- | Translate `MonadHttp` to `FreeHttp` free monad.
instance MonadHttp FreeHttp where
    httpLbs r m = liftF (HttpLbs r m id)

-- | Run `FreeHttp` sequence with mocked response.
runPure :: HashMap String (Response LBS.ByteString) -> FreeHttp a -> a
runPure _ (Pure x)                 = x
runPure m (Free (HttpLbs req _ f)) =
    case HM.lookup path m of
        Just res -> runPure m $ f res
        Nothing  -> error $ "no responses matched for path " <> path
  where
    protocol = if Ht.secure req then "https://" else "http://"
    path     = protocol <> C.unpack (Ht.host req <> Ht.path req)

-- | Run HTTP request and parse the response into error or `FromJson` data.
httpJson
    :: (FromJSON a, MonadHttp m)
    => ErrorHandler -> Manager -> Request -> m (Either String a)
httpJson handler manager request =
    mapEitherDecode handler <$> httpLbs request manager
