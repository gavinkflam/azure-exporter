{-# LANGUAGE OverloadedStrings #-}

module Auth
    (
      -- * OAuth2
      acquireToken
      -- * STM
    , getTokenOrRaise
    , refreshTokenIfExpired
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, addUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)

import Control.Lens ((?~), (^.))
import Network.HTTP.Client (Manager)

import Control.Monad.AppEnvSTM
import Control.Monad.Either (raiseLeft)
import Control.Monad.Maybe (raiseIfNothing)
import qualified Data.App.AccessToken as T
import qualified Data.App.AppEnv as E
import qualified Data.App.Config as C
import Data.OAuth2.AcquireAccessTokenRequest as AT
import qualified Data.OAuth2.AcquireAccessTokenResponse as R
import HTTP (IOResponse, requestIO)
import Types (AppAction)

-- | Acquire a token from Azure with credentials in `Config`.
--
--   A HTTP `Manager` will be required.
acquireToken :: C.Config -> Manager -> IOResponse R.AcquireAccessTokenResponse
acquireToken conf manager = do
    let params = AT.Params
          { AT._clientId     = conf ^. C.clientId
          , AT._clientSecret = conf ^. C.clientSecret
          , AT._tenantId     = conf ^. C.tenantId
          }
    requestIO manager AT.errorExtractor $ AT.request params

-- | Get the auth token from the shared `AppEnv`, raise if not found.
getTokenOrRaise :: AppAction Text
getTokenOrRaise = do
    mToken <- liftSTM $ (^. E.accessToken) <$> readAppEnv
    token  <- raiseIfNothing "Authorization token not found" mToken
    return $ token ^. T.accessToken

-- | Refresh the auth token in the shared `AppEnv` if the token has expired.
--
--   Do nothing if the token is still valid.
refreshTokenIfExpired :: AppAction ()
refreshTokenIfExpired = do
    mToken  <- liftSTM $ (^. E.accessToken) <$> readAppEnv
    expired <- liftIO $ maybe (return True) (tokenExpired 10) mToken
    when expired refreshToken

-- | Refresh the auth token in the shared `AppEnv`.
refreshToken :: AppAction ()
refreshToken = do
    conf    <- liftSTM $ (^. E.config) <$> readAppEnv
    manager <- liftSTM $ (^. E.httpManager) <$> readAppEnv
    resp    <- raiseLeft =<< liftIO (acquireToken conf manager)
    liftSTM $ modifyAppEnv (E.accessToken ?~ T.fromResponse resp)

-- | Check if the given `AccessToken` is still valid.
tokenExpired :: NominalDiffTime -> T.AccessToken -> IO Bool
tokenExpired offset t = do
    now <- getSystemTime
    return $ (t ^. T.expiresOn) < addUTCTime (- offset) (systemToUTCTime now)
