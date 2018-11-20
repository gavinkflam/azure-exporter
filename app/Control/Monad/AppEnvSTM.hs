{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- STM varialbe reader Monad for sharing `AppEnv`.
--
-- Inspired from Scotty global state example.
--
-- <https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs>
module Control.Monad.AppEnvSTM
  -- * Monad
  ( AppEnvSTM (..)
  , liftSTM
  , runAppEnvSTMIntoIO
  -- * STM
  , readAppEnv
  , modifyAppEnv
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVarIO)
import Control.Monad.Reader

import Data.AppEnv (AppEnv)

-- | Reader monad providing the `AppEnv` STM variable.
newtype AppEnvSTM a = AppEnvSTM
  { runAppEnvTVarReader :: ReaderT (TVar AppEnv) IO a
  } deriving (Applicative, Functor, Monad, MonadIO ,MonadReader (TVar AppEnv))

-- | Lift a computation from the `AppEnvSTM` monad.
liftSTM :: MonadTrans t => AppEnvSTM a -> t AppEnvSTM a
liftSTM = lift

-- | Run the `AppEnvSTM` effect into `IO` effect.
runAppEnvSTMIntoIO :: TVar AppEnv -> AppEnvSTM a -> IO a
runAppEnvSTMIntoIO var a = runReaderT (runAppEnvTVarReader a) var

-- | Read the shared `AppEnv` from STM.
readAppEnv :: AppEnvSTM AppEnv
readAppEnv = ask >>= liftIO . readTVarIO

-- | Update the shared `AppEnv` to STM.
modifyAppEnv :: (AppEnv -> AppEnv) -> AppEnvSTM ()
modifyAppEnv f = ask >>= liftIO . atomically . flip modifyTVar' f
