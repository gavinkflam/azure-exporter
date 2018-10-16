module AzureExporterExe.UsageDump
  -- * Usage
  ( dumpUsage
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Network.HTTP.Client (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           AzureExporterExe.Auth (acquireToken)
import           AzureExporterExe.Control.Monad.Either (dieLeft)
import qualified AzureExporterExe.Data.AccessToken as T
import           AzureExporterExe.Data.Config (getConfig)
import           AzureExporterExe.HTTP (requestIO)

-- | Dump usage data in CSV format.
dumpUsage :: IO ()
dumpUsage = do
  config   <- getConfig
  manager  <- newManager tlsManagerSettings
  tokenRes <- dieLeft =<< liftIO (acquireToken config manager)

  print $ show $ T.fromResponse tokenRes
  return ()
