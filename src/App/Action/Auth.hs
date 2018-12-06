module App.Action.Auth
    (
      refreshTokenIfExpired
    , refreshToken
    ) where

import Control.Concurrent.STM.TVar (TVar)
import Control.Monad (when)
import Control.Monad.Fail (MonadFail)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.Clock.System (systemToUTCTime)

import Control.Lens ((^.))
import Network.HTTP.Client (Manager)

import Control.Monad.Fail.Trans (failLeft)
import Control.Monad.Network.MonadHttp (MonadHttp, httpJson)
import Control.Monad.STM.Class (MonadTVarReader(..), MonadTVarWriter(..))
import Control.Monad.System.MonadTime (MonadTime(..))
import qualified Data.App.AccessToken as T
import qualified Data.App.Config as Cf
import Data.OAuth2.AcquireAccessTokenRequest as AT

-- | Refresh the auth token in the shared `AppEnv` if the token has expired.
--
--   Do nothing if the token is still valid.
refreshTokenIfExpired
    :: (MonadFail m, MonadHttp m, MonadTime m, MonadTVarReader m, MonadTVarWriter m)
    => Manager -> Cf.Config -> TVar T.AccessToken -> m ()
refreshTokenIfExpired manager conf tokenTVar = do
    expired <- tokenExpired 10 =<< readTVar tokenTVar
    when expired $ refreshToken manager conf tokenTVar

-- | Refresh the auth token and update the `TVar`.
refreshToken
    :: (MonadFail m, MonadHttp m, MonadTVarWriter m)
    => Manager -> Cf.Config -> TVar T.AccessToken -> m ()
refreshToken manager conf tokenTVar = do
    resp <- failLeft =<< httpJson AT.errorExtractor manager (AT.request params)
    writeTVar tokenTVar $ T.fromResponse resp
  where
    params = AT.Params
        { AT._clientId     = conf ^. Cf.clientId
        , AT._clientSecret = conf ^. Cf.clientSecret
        , AT._tenantId     = conf ^. Cf.tenantId
        }

-- | Check if the given token is still valid.
tokenExpired :: MonadTime m => NominalDiffTime -> T.AccessToken -> m Bool
tokenExpired offset t = do
    now <- getSystemTime
    return $ (t ^. T.expiresOn) < addUTCTime (- offset) (systemToUTCTime now)
