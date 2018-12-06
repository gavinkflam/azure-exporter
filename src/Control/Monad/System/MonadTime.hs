{-# LANGUAGE DeriveFunctor, FlexibleInstances, TypeSynonymInstances #-}

-- | System time reading monad.
module Control.Monad.System.MonadTime
    (
      -- * Monad
      MonadTime (..)
      -- * Free Monad Interpreter
    , runPure
    ) where

import Data.Time.Clock.System (SystemTime)
import qualified Data.Time.Clock.System as Cl

import Control.Monad.Free (Free(..), liftF)

-- | System time reading monad.
class Monad m => MonadTime m where
    getSystemTime :: m SystemTime

-- | Implement `MonadTime` as `IO`.
instance MonadTime IO where
    getSystemTime = Cl.getSystemTime

-- | System time reading operations represented as functors.
newtype FreeTimeF x
    = GetSystemTime (SystemTime -> x)
    deriving Functor

-- | System time reading oprations contained as free monad.
type FreeTime = Free FreeTimeF

-- | Translate `MonadTime` to `FreeTime` free monad.
instance MonadTime FreeTime where
    getSystemTime = liftF $ GetSystemTime id

-- | Run `FreeTime` sequence with mocked system time.
runPure :: SystemTime -> FreeTime a -> a
runPure _ (Pure x)                 = x
runPure t (Free (GetSystemTime f)) = runPure t $ f t
