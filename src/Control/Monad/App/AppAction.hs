{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.App.AppAction
    (
      -- * Types
      AppAction
    ) where

import qualified Control.Concurrent.STM.TVar as Tv
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader)
import qualified Control.Monad.Reader as Rd
import Control.Monad.STM (atomically)
import qualified Data.Text.Lazy as LT
import qualified Data.Time.Clock.System as Cl

import qualified Network.HTTP.Client as Ht
import Web.Scotty.Trans (ActionT)

import Control.Monad.App.AppM (AppM)
import Control.Monad.Network.MonadHttp (MonadHttp(..))
import Control.Monad.STM.Class (MonadTVarReader(..), MonadTVarWriter(..))
import Control.Monad.System.MonadTime (MonadTime(..))
import Data.App.AppEnv (AppEnv)

-- | Type for Scotty routes.  The stack will provide AppEnv reading via AppM.
type AppAction = ActionT LT.Text AppM

-- | Implement `MonadHttp` for `AppAction`.
instance MonadHttp AppAction where
    httpLbs r m = liftIO $ Ht.httpLbs r m

-- | Implement `MonadReader` for `AppAction`.
instance (MonadReader AppEnv) AppAction where
    ask    = Rd.lift Rd.ask
    local  = Rd.local
    reader = Rd.reader

-- | Implement `MonadTVarReader` for `AppAction`.
instance MonadTVarReader AppAction where
    readTVar = liftIO . Tv.readTVarIO

-- | Implement `MonadTVarWriter` for `AppAction`.
instance MonadTVarWriter AppAction where
    writeTVar var new = liftIO $ atomically $ Tv.writeTVar var new

-- | Implement `MonadTime` for `AppAction`.
instance MonadTime AppAction where
    getSystemTime = liftIO Cl.getSystemTime
