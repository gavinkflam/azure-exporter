{-# LANGUAGE FlexibleContexts #-}

module App.Arg.Monitor
    (
      -- * Usage
      assembleArgs
    ) where

import Control.Monad.Reader (MonadReader, ask)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.System (systemToUTCTime)

import Network.HTTP.Client (Manager)

import Control.Monad.STM.Class (MonadTVarReader(..))
import Control.Monad.System.MonadTime (MonadTime(..))
import qualified Data.App.AccessToken as Ak
import qualified Data.App.AppEnv as En
import qualified Data.Monitor.ListMetricValuesRequest as Lm
import Text.Monitor.Timespan (timespanFrom)

-- | Assemble the arguments for fetching monitor gauges.
assembleArgs
    :: (MonadReader En.AppEnv m, MonadTVarReader m, MonadTime m)
    => Text -> Text -> Text -> m (Manager, Text, Lm.Params)
assembleArgs target metricNames aggregation = do
    env   <- ask
    token <- readTVar $ En.accessToken env
    now   <- getSystemTime

    return
        ( En.httpManager env
        , Ak.token token
        , metricParams target metricNames aggregation $ systemToUTCTime now
        )

-- | Construct params for `ListMetricValuesRequest`.
metricParams :: Text -> Text -> Text -> UTCTime -> Lm.Params
metricParams target metricNames aggregation now = Lm.Params
    { Lm.aggregation = aggregation
    , Lm.metricNames = metricNames
    , Lm.resourceId  = target
    , Lm.timespan    = T.pack $ timespanFrom now 150 90
    }
