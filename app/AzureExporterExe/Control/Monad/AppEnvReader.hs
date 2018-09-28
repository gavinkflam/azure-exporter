{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- Modified from Scotty global state example
-- https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
module AzureExporterExe.Control.Monad.AppEnvReader
  ( AppEnvReader (..)
  , getR
  , liftR
  , runReaderIntoIO
  ) where

import AzureExporterExe.Data.AppEnv (AppEnv)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.Reader

newtype AppEnvReader a =
  AppEnvReader { runAppEnvReader :: ReaderT (TVar AppEnv) IO a
               } deriving ( Applicative, Functor, Monad, MonadIO
                          , MonadReader (TVar AppEnv)
                          )

getR :: (AppEnv -> a) -> AppEnvReader a
getR f = ask >>= liftIO . readTVarIO >>= return . f

liftR :: MonadTrans t => AppEnvReader a -> t AppEnvReader a
liftR = lift

runReaderIntoIO :: TVar AppEnv -> AppEnvReader a -> IO a
runReaderIntoIO var a = runReaderT (runAppEnvReader a) var
