{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Monitor
    (
      -- * Routes
      metrics
    ) where

import Control.Monad.Reader (ask)
import Data.Tuple.Curry (uncurryN)
import Web.Scotty.Trans (param)

import App.Action.Gauge (respGauges)
import qualified App.Arg.Auth as Aau
import qualified App.Arg.Monitor as Arm
import qualified App.Http.Auth as Hau
import qualified App.Http.Monitor as Htm
import Control.Monad.App.AppAction (AppAction)

-- | Route for Azure Monitor metrics.
metrics :: AppAction ()
metrics = do
    appEnv      <- ask
    target      <- param "target"
    metricNames <- param "metricNames"
    aggregation <- param "aggregation"

    uncurryN Hau.refreshTokenIfExpired $ Aau.assembleArgs appEnv

    respGauges =<< uncurryN Htm.fetchGauges =<<
        Arm.assembleArgs target metricNames aggregation
