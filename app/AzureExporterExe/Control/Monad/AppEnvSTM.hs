{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Modified from Scotty global state example
-- https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
module AzureExporterExe.Control.Monad.AppEnvSTM
  ( AppEnvSTM (..)
  -- Transform
  , liftSTM
  , runAppEnvSTMIntoIO
  -- STM
  , readAppEnv
  ) where

import AzureExporterExe.Data.AppEnv (AppEnv)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.Reader

newtype AppEnvSTM a =
  AppEnvSTM { runAppEnvTVarReader :: ReaderT (TVar AppEnv) IO a
            } deriving ( Applicative, Functor, Monad, MonadIO
                       , MonadReader (TVar AppEnv)
                       )

-- Transform
liftSTM :: MonadTrans t => AppEnvSTM a -> t AppEnvSTM a
liftSTM = lift

runAppEnvSTMIntoIO :: TVar AppEnv -> AppEnvSTM a -> IO a
runAppEnvSTMIntoIO var a = runReaderT (runAppEnvTVarReader a) var

-- STM
readAppEnv :: AppEnvSTM AppEnv
readAppEnv = ask >>= liftIO . readTVarIO
