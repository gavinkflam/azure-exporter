{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

module App.Arg.Billing
    (
      -- * Usage
      assembleArgs
    ) where

import Control.Monad.Reader (MonadReader, ask)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Lens ((^.))
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
    token <- readTVar (env ^. En.accessToken)

    let config  = env ^. En.config
        manager = env ^. En.httpManager
        aParams = usagesParams config (T.pack startTime) (T.pack endTime)
        gParams = rateCardParams config

    return (manager, token ^. Ak.token, aParams, gParams)

-- | Construct params for `UsageAggregateRequest`.
usagesParams :: Cf.Config -> Text -> Text -> A.Params
usagesParams config startTime endTime = A.Params
    { A._subscriptionId         = config ^. Cf.subscriptionId
    , A._aggregationGranularity = "daily"
    , A._reportedStartTime      = startTime
    , A._reportedEndTime        = endTime
    , A._continuationToken      = Nothing
    }

-- | Construct params for `GetRateCardRequest`.
rateCardParams :: Cf.Config -> G.Params
rateCardParams config = G.Params
    { G._subscriptionId = config ^. Cf.subscriptionId
    , G._offerId        = config ^. Cf.offerId
    , G._currency       = config ^. Cf.currency
    , G._locale         = config ^. Cf.locale
    , G._regionInfo     = config ^. Cf.regionInfo
    }
