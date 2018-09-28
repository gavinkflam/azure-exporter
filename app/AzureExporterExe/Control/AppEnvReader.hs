{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Modified from Scotty global state example
-- https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
module AzureExporterExe.Control.AppEnvReader
  ( AppEnvReader (..)
  , get
  , liftR
  , runIntoIO
  ) where

import AzureExporterExe.Data.AppEnv (AppEnv)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.Reader

newtype AppEnvReader a =
  AppEnvReader { runAppEnvReader :: ReaderT (TVar AppEnv) IO a
               } deriving ( Applicative, Functor, Monad, MonadIO
                          , MonadReader (TVar AppEnv)
                          )

get :: (AppEnv -> a) -> AppEnvReader a
get f = ask >>= liftIO . readTVarIO >>= return . f

liftR :: MonadTrans t => AppEnvReader a -> t AppEnvReader a
liftR = lift

runIntoIO :: TVar AppEnv -> AppEnvReader a -> IO a
runIntoIO var a = runReaderT (runAppEnvReader a) var
