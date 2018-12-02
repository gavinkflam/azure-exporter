{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

-- | Environment variables reading monad.
module Control.Monad.System.EnvM
    (
      -- * Monad
      EnvM (..)
      -- * Free Monad Interpreter
    , runPure
    ) where

import qualified System.Environment as Ev

import Control.Monad.Free (Free(..), liftF)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- | Environment variables reading monad.
class Monad m => EnvM m where
    getEnv    :: String -> m String
    lookupEnv :: String -> m (Maybe String)

-- | Implement `EnvM` as `IO`.
instance EnvM IO where
    getEnv    = Ev.getEnv
    lookupEnv = Ev.lookupEnv

-- | Environment variables reading operations represented as functors.
data FreeEnvMF x
    = GetEnv String (String -> x)
    | LookupEnv String (Maybe String -> x)
    deriving Functor

-- | Environment variable reading oprations contained as free monad.
type FreeEnvM = Free FreeEnvMF

-- | Translate `EnvM` to `FreeEnvM` free monad.
instance EnvM FreeEnvM where
    getEnv k    = liftF (GetEnv k id)
    lookupEnv k = liftF (LookupEnv k id)

-- | Run `FreeEnvM` sequence with mocked environment vairables.
runPure :: HashMap String String -> FreeEnvM a -> a
runPure _ (Pure x)               = x
runPure m (Free (GetEnv k f))    =
    case HM.lookup k m of
        Nothing -> error $ k <> ": getEnv: does not exist"
        Just v  -> runPure m $ f v
runPure m (Free (LookupEnv k f)) = runPure m $ f $ HM.lookup k m
