{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module App.Arg.Billing
    (
      -- * Usage
      assembleArgs
    ) where

import Control.Monad.Reader (MonadReader, ask)
import Data.Text (Text)
import qualified Data.Text as T

import Network.HTTP.Client (Manager)

import Control.Monad.STM.Class (MonadTVarReader(..))
import qualified Data.App.AccessToken as Ak
import qualified Data.App.AppEnv as En
import qualified Data.App.Config as Cf
import qualified Data.Billing.GetRateCardRequest as G
import qualified Data.Billing.ListUsageAggregatesRequest as A

-- | Assemble the arguments for fetching billing gauges.
assembleArgs
    :: (MonadReader En.AppEnv m, MonadTVarReader m)
    => String -> String -> m (Manager, Text, A.Params, G.Params)
assembleArgs startTime endTime = do
    env   <- ask
    token <- readTVar $ En.accessToken env

    let config  = En.config env
        manager = En.httpManager env
        aParams = usagesParams config (T.pack startTime) (T.pack endTime)
        gParams = rateCardParams config

    return (manager, Ak.token token, aParams, gParams)

-- | Construct params for `UsageAggregateRequest`.
usagesParams :: Cf.Config -> Text -> Text -> A.Params
usagesParams config startTime endTime = A.Params
    { A.subscriptionId         = Cf.subscriptionId config
    , A.aggregationGranularity = "daily"
    , A.reportedStartTime      = startTime
    , A.reportedEndTime        = endTime
    , A.continuationToken      = Nothing
    }

-- | Construct params for `GetRateCardRequest`.
rateCardParams :: Cf.Config -> G.Params
rateCardParams config = G.Params
    { G.subscriptionId = Cf.subscriptionId config
    , G.offerId        = Cf.offerId config
    , G.currency       = Cf.currency config
    , G.locale         = Cf.locale config
    , G.regionInfo     = Cf.regionInfo config
    }
