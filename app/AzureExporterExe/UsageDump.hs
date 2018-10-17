{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.UsageDump
  -- * Usage
  ( dumpUsage
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Lens ((^.))
import           Data.Text.Lazy (Text)

import           Network.HTTP.Client (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Azure.Data.Aeson.Parser (errorExtractor)
import qualified Azure.Data.Billing.ListUsageAggregatesResponse as R
import qualified Azure.Data.Billing.UsageAggregate as U
import qualified Azure.Request.Billing.ListUsageAggregates as A
import           AzureExporterExe.Auth (acquireToken)
import           AzureExporterExe.Control.Monad.Either (dieLeft)
import qualified AzureExporterExe.Data.AccessToken as T
import qualified AzureExporterExe.Data.Config as C
import           AzureExporterExe.HTTP (requestIO)

-- | Dump usage data in CSV format.
dumpUsage :: IO ()
dumpUsage = do
  config   <- C.getConfig
  manager  <- newManager tlsManagerSettings
  tokenRes <- dieLeft =<< liftIO (acquireToken config manager)

  let token  = T.fromResponse tokenRes ^. T.accessToken
      params = A.Params { A._subscriptionId         = config ^. C.subscriptionId
                        , A._aggregationGranularity = "daily"
                        , A._reportedStartTime      = "2018-06-01T00:00:00+00:00"
                        , A._reportedEndTime        = "2018-10-01T00:00:00+00:00"
                        , A._continuationToken      = Nothing
                        }
  usages <- fetchUsages manager token params
  print $ show $ length usages

  return ()

-- |
-- Fetch usage aggregates from Azure while recursively fetching with the
-- continuation token if any.
fetchUsages :: Manager -> Text -> A.Params -> IO [U.UsageAggregate]
fetchUsages manager token params = do
  let req = A.request token params
  res <- dieLeft =<< liftIO (requestIO manager errorExtractor req)

  case res ^. R.nextLink of
    Nothing -> return (res ^. R.value)
    Just _  -> (++ (res ^. R.value)) <$> fetchUsages manager token nParams
      where nParams = params { A._continuationToken = R.continuationToken res }
