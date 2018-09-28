{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.App
  ( app
  ) where

import           AzureExporterExe.Control.Monad.AppEnvReader
import qualified AzureExporterExe.Route.Monitor as M
import           Data.Text.Lazy (Text)
import           Web.Scotty.Trans (ScottyT, get)

app :: ScottyT Text AppEnvReader ()
app = do
  get "/monitor/metrics" M.metrics
