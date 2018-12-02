{-# LANGUAGE OverloadedStrings #-}

module UsageDump
    (
      -- * Usage
      dumpUsage
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy (putStr)
import Data.List (sort)
import Data.Text (Text, pack)
import Prelude hiding (putStr)

import Control.Lens ((^.))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Auth (acquireToken)
import Control.Monad.Either (dieLeft)
import qualified Control.Monad.System.EnvM as Em
import qualified Data.App.AccessToken as T
import qualified Data.App.Config as C
import qualified Data.Billing.GetRateCardRequest as G
import qualified Data.Billing.GetRateCardResponse as GR
import qualified Data.Billing.ListUsageAggregatesRequest as A
import qualified Data.Billing.ListUsageAggregatesResponse as AR
import qualified Data.Billing.UsageAggregate as U
import Data.Csv.IncrementMod (encodeNamedRecords)
import Data.Response.Aeson (errorExtractor)
import HTTP (requestIO)

-- | Dump usage data in CSV format.
dumpUsage :: String -> String -> IO ()
dumpUsage startTime endTime = do
    config   <- Em.runIntoIO C.getConfig
    manager  <- newManager tlsManagerSettings
    tokenRes <- dieLeft =<< liftIO (acquireToken config manager)

    let token   = T.fromResponse tokenRes ^. T.token
        aParams = A.Params
            { A._subscriptionId         = config ^. C.subscriptionId
            , A._aggregationGranularity = "daily"
            , A._reportedStartTime      = pack startTime
            , A._reportedEndTime        = pack endTime
            , A._continuationToken      = Nothing
            }
        gParams = G.Params
            { G._subscriptionId = config ^. C.subscriptionId
            , G._offerId        = config ^. C.offerId
            , G._currency       = config ^. C.currency
            , G._locale         = config ^. C.locale
            , G._regionInfo     = config ^. C.regionInfo
            }
    usages   <- fetchUsages manager token aParams
    rateCard <- fetchRateCard manager token gParams
    putStr $ encodeNamedRecords $ sort $ U.toGauges rateCard usages

-- | Fetch usage aggregates from Azure while recursively fetching with the
--   continuation token if any.
fetchUsages :: Manager -> Text -> A.Params -> IO [U.UsageAggregate]
fetchUsages manager token params = do
    let req = A.request token params
    res <- dieLeft =<< liftIO (requestIO manager errorExtractor req)

    case res ^. AR.nextLink of
        Nothing -> return (res ^. AR.value)
        Just _  ->
            (++ (res ^. AR.value)) <$> fetchUsages manager token nParams
          where
            nParams = params
                { A._continuationToken = AR.continuationToken res }

-- | Fetch rate card from Azure.
fetchRateCard :: Manager -> Text -> G.Params -> IO GR.GetRateCardResponse
fetchRateCard manager token params = do
    let req = G.request token params
    dieLeft =<< liftIO (requestIO manager errorExtractor req)
