{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- Modified from Scotty global state example
-- https://github.com/scotty-web/scotty/blob/master/examples/globalstate.hs
module AzureExporterExe.Control.AppState
  ( AppState (..)
  -- Lenses
  , config
  , accessToken
  -- WebM
  , WebM (..)
  , webM
  , get
  , runWebMIntoIO
  ) where

import AzureExporterExe.Data.Config (Config)
import AzureExporterExe.Data.AccessToken (AccessToken)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Lens (makeLenses, (^.))
import Control.Monad.Reader

data AppState =
  AppState { _config      :: Config
           , _accessToken :: AccessToken
           } deriving Show

makeLenses ''AppState

newtype WebM a =
  WebM { runWebM :: ReaderT (TVar AppState) IO a
       } deriving ( Applicative, Functor, Monad, MonadIO
                  , MonadReader (TVar AppState)
                  )

webM :: MonadTrans t => WebM a -> t WebM a
webM = lift

get :: (AppState -> a) -> WebM a
get f = ask >>= liftIO . readTVarIO >>= return . f

runWebMIntoIO :: TVar AppState -> WebM a -> IO a
runWebMIntoIO var a = runReaderT (runWebM a) var
