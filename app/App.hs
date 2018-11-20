{-# LANGUAGE OverloadedStrings #-}

module App
  -- * Scotty App
  ( app
  ) where

import           Control.Monad.AppEnvSTM (AppEnvSTM)
import qualified Data.Route.Monitor as M
import           Data.Text.Lazy (Text)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans (ScottyT, get, middleware)

-- | Scotty application for the exporter HTTP server.
app :: ScottyT Text AppEnvSTM ()
app = do
  middleware logStdout
  -- Monitor
  get "/monitor/metrics" M.metrics
