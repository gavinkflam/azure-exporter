{-# LANGUAGE OverloadedStrings #-}

module Data.Route.Monitor
    (
      -- * Routes
      metrics
    ) where

import Data.Tuple.Curry (uncurryN)
import Web.Scotty.Trans (param)

import App.Action.Gauge (respGauges)
import qualified App.Arg.Monitor as Arm
import qualified App.Http.Monitor as Htm
import Types (AppAction)

-- | Route for Azure Monitor metrics.
metrics :: AppAction ()
metrics = do
    target      <- param "target"
    metricNames <- param "metricNames"
    aggregation <- param "aggregation"

    respGauges =<< uncurryN Htm.fetchGauges =<<
        Arm.assembleArgs target metricNames aggregation
