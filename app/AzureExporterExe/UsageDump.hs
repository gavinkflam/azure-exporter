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
import qualified Azure.Data.Billing.GetRateCardResponse as GR
import qualified Azure.Data.Billing.ListUsageAggregatesResponse as AR
import qualified Azure.Data.Billing.Meter as M
import qualified Azure.Data.Billing.UsageAggregate as U
import qualified Azure.Request.Billing.GetRateCard as G
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

  let token   = T.fromResponse tokenRes ^. T.accessToken
      aParams = A.Params { A._subscriptionId         = config ^. C.subscriptionId
                         , A._aggregationGranularity = "daily"
                         , A._reportedStartTime      = "2018-06-01T00:00:00+00:00"
                         , A._reportedEndTime        = "2018-10-01T00:00:00+00:00"
                         , A._continuationToken      = Nothing
                         }
      gParams = G.Params { G._subscriptionId = config ^. C.subscriptionId
                         , G._offerId        = "MS-AZR-0003p"
                         , G._currency       = "USD"
                         , G._locale         = "en-US"
                         , G._regionInfo     = "US"
                         }
  usages <- fetchUsages manager token aParams
  print $ show $ length usages
  print $ show $ head usages

  meters <- fetchMeters manager token gParams
  print $ show $ length meters
  print $ show $ head meters

  return ()

-- |
-- Fetch usage aggregates from Azure while recursively fetching with the
-- continuation token if any.
fetchUsages :: Manager -> Text -> A.Params -> IO [U.UsageAggregate]
fetchUsages manager token params = do
  let req = A.request token params
  res <- dieLeft =<< liftIO (requestIO manager errorExtractor req)

  case res ^. AR.nextLink of
    Nothing -> return (res ^. AR.value)
    Just _  -> (++ (res ^. AR.value)) <$> fetchUsages manager token nParams
      where nParams = params { A._continuationToken = AR.continuationToken res }

-- | Fetch rate card meters from Azure.
fetchMeters :: Manager -> Text -> G.Params -> IO [M.Meter]
fetchMeters manager token params = do
  let req = G.request token params
  res <- dieLeft =<< liftIO (requestIO manager errorExtractor req)
  return (res ^. GR.meters)
