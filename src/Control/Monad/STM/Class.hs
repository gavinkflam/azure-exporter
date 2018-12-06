{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Control.Monad.STM.Class
    ( MonadTVarReader (..)
    , MonadTVarWriter (..)
    ) where

import Control.Concurrent.STM.TVar (TVar)
import qualified Control.Concurrent.STM.TVar as Tv
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)

import Control.Monad.App.AppM (AppM)

class Monad m => MonadTVarReader m where
    readTVar :: TVar a -> m a

instance MonadTVarReader IO where
    readTVar = Tv.readTVarIO

instance MonadTVarReader AppM where
    readTVar = liftIO . Tv.readTVarIO

class Monad m => MonadTVarWriter m where
    writeTVar :: TVar a -> a -> m ()

instance MonadTVarWriter IO where
    writeTVar var new = atomically $ Tv.writeTVar var new

instance MonadTVarWriter AppM where
    writeTVar var new = liftIO $ atomically $ Tv.writeTVar var new
