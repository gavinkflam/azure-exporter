module App.Billing.Cost
    (
      -- * Request
      fetchUsageAndCostGauges
    ) where

import Control.Monad.Fail (MonadFail)
import Data.List (sort)
import Data.Text (Text)

import Control.Lens ((^.))
import Network.HTTP.Client (Manager)

import Control.Monad.Fail.Trans (failLeft)
import Control.Monad.Network.HttpM (HttpM, httpJson)
import qualified Data.Billing.GetRateCardRequest as G
import qualified Data.Billing.GetRateCardResponse as GR
import qualified Data.Billing.ListUsageAggregatesRequest as A
import qualified Data.Billing.ListUsageAggregatesResponse as AR
import qualified Data.Billing.UsageAggregate as U
import Data.Prometheus.Gauge (Gauge)
import Data.Response.Aeson (errorExtractor)

-- | Fetch usage aggregates and rete card from Azure, then derive the gauges.
fetchUsageAndCostGauges
    :: (HttpM m, MonadFail m)
    => Manager -> Text -> A.Params -> G.Params -> m [Gauge]
fetchUsageAndCostGauges manager token aParams gParams = do
    usages   <- fetchUsageAggregates manager token aParams
    rateCard <- fetchRateCard manager token gParams
    return $ sort $ U.toGauges rateCard usages

-- | Fetch usage aggregates from Azure.
--
--   If there is any continuation token, we will keep fetching until done.
fetchUsageAggregates
    :: (HttpM m, MonadFail m)
    => Manager -> Text -> A.Params -> m [U.UsageAggregate]
fetchUsageAggregates manager token params = do
    res <- failLeft =<< httpJson errorExtractor manager (A.request token params)

    case res ^. AR.nextLink of
        Nothing -> return (res ^. AR.value)
        Just _  -> (++ (res ^. AR.value)) <$>
            fetchUsageAggregates manager token
            (params { A._continuationToken = AR.continuationToken res })

-- | Fetch rate card from Azure.
fetchRateCard
    :: (HttpM m, MonadFail m)
    => Manager -> Text -> G.Params -> m GR.GetRateCardResponse
fetchRateCard manager token params =
    failLeft =<< httpJson errorExtractor manager (G.request token params)
