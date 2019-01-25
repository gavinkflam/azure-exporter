{-# LANGUAGE OverloadedStrings #-}

module App
    (
      -- * Scotty App
      app
    ) where

import Data.Text.Lazy (Text)

import Control.Monad.App.AppM (AppM)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (ScottyT, get, middleware)

import qualified Data.Route.Monitor as Rom
import qualified Data.Route.Ping as V

-- | Scotty application for the exporter HTTP server.
app :: ScottyT Text AppM ()
app = do
    middleware logStdout
    get "/monitor/metrics" Rom.metrics
    get "/ping" V.ping
