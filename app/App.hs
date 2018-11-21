{-# LANGUAGE OverloadedStrings #-}

module App
    (
      -- * Scotty App
      app
    ) where

import Data.Text.Lazy (Text)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Web.Scotty.Trans (ScottyT, get, middleware)

import Control.Monad.AppEnvSTM (AppEnvSTM)
import qualified Data.Route.Monitor as M

-- | Scotty application for the exporter HTTP server.
app :: ScottyT Text AppEnvSTM ()
app = do
    middleware logStdout
    get "/monitor/metrics" M.metrics  -- Monitor
