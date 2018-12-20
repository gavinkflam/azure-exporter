{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Ping
    (
      -- * Routes
      ping
    ) where

import Web.Scotty.Trans (raw)

import Control.Monad.App.AppAction (AppAction)

-- | Service status probe.
ping :: AppAction ()
ping = raw "pong"
