{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Billing
    (
      -- * Routes
      metrics
    ) where

import Control.Monad.Reader (ask)
import Data.Tuple.Curry (uncurryN)
import Web.Scotty.Trans (param)

import App.Action.Gauge (respGauges)
import qualified App.Arg.Auth as Aau
import qualified App.Arg.Billing as Arb
import qualified App.Http.Auth as Hau
import qualified App.Http.Billing as Htb
import Control.Monad.App.AppAction (AppAction)

-- | Route for Azure Billing metrics.
metrics :: AppAction ()
metrics = do
    appEnv    <- ask
    startTime <- param "startTime"
    endTime   <- param "endTime"

    uncurryN Hau.refreshTokenIfExpired $ Aau.assembleArgs appEnv

    respGauges =<< uncurryN Htb.fetchGauges =<<
        Arb.assembleArgs startTime endTime
