{-# LANGUAGE DeriveFunctor #-}

-- | Environment variable reading contained as free monad.
module Control.Monad.System.EnvM
    (
      -- * Monad
      EnvM
      -- * Free IO functions
    , getEnv
    , lookupEnv
      -- * Interpreter
    , runIntoIO
    , runPure
    ) where

import qualified System.Environment as Ev

import Control.Monad.Free (Free(..), liftF)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

-- | Environment variable reading operations represented as functors.
data EnvMF x
    = GetEnv String (String -> x)
    | LookupEnv String (Maybe String -> x)
    deriving Functor

-- | Environment variable reading oprations contained as free monad.
type EnvM = Free EnvMF

-- | Return environment with the the given name or fail.
getEnv :: String -> EnvM String
getEnv k = liftF $ GetEnv k id

-- | Return environment with the the given name or `Nothing`.
lookupEnv :: String -> EnvM (Maybe String)
lookupEnv k = liftF $ LookupEnv k id

-- | Run `EnvM` sequence into IO.
runIntoIO :: EnvM a -> IO a
runIntoIO (Pure x)               = return x
runIntoIO (Free (GetEnv k f))    = Ev.getEnv k >>= runIntoIO . f
runIntoIO (Free (LookupEnv k f)) = Ev.lookupEnv k >>= runIntoIO . f

-- | Run `EnvM` sequence with mocked map.
runPure :: HashMap String String -> EnvM a -> a
runPure _ (Pure x)               = x
runPure m (Free (GetEnv k f))    =
    case HM.lookup k m of
        Nothing -> error $ k <> ": getEnv: does not exist"
        Just v  -> runPure m $ f v
runPure m (Free (LookupEnv k f)) = runPure m $ f $ HM.lookup k m
