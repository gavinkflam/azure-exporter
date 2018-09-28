{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.App
  ( app
  ) where

import           AzureExporterExe.Control.Monad.AppEnvSTM (AppEnvSTM)
import qualified AzureExporterExe.Route.Monitor as M
import           Data.Text.Lazy (Text)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           Web.Scotty.Trans (ScottyT, get, middleware)

app :: ScottyT Text AppEnvSTM ()
app = do
  middleware logStdout
  -- Monitor
  get "/monitor/metrics" M.metrics
