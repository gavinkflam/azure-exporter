module App.Http.Monitor
    (
      -- * Request
      fetchGauges
    ) where

import Control.Monad.Fail (MonadFail)
import Data.List (sort)
import Data.Text (Text)

import Network.HTTP.Client (Manager)

import Control.Monad.Fail.Trans (failLeft)
import Control.Monad.Network.MonadHttp (MonadHttp, httpJson)
import qualified Data.Monitor.ListMetricValuesRequest as Lm
import qualified Data.Monitor.ListMetricValuesResponse as Lmr
import Data.Prometheus.Gauge (Gauge)
import Data.Response.Aeson (errorExtractor)

-- | Fetch usage aggregates and rete card from Azure, then derive the gauges.
fetchGauges
    :: (MonadFail m, MonadHttp m) => Manager -> Text -> Lm.Params -> m [Gauge]
fetchGauges manager token params = do
    vs <- failLeft =<< httpJson errorExtractor manager (Lm.request token params)
    return $ sort $ Lmr.toGauges vs
