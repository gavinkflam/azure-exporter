{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Auth
  ( getTokenOrRaise
  , refreshTokenIfExpired
  ) where

import           AzureExporterExe.Control.Monad.AppEnvSTM
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import           AzureExporterExe.Control.Monad.Maybe (raiseIfNothing)
import qualified AzureExporterExe.Data.AccessToken as T
import qualified AzureExporterExe.Data.AppEnv as E
import qualified AzureExporterExe.Data.Config as C
import           Azure.Request.OAuth2.AcquireAccessToken as AT
import           Control.Lens ((^.), (&), (.~))
import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text.Lazy (Text)
import           Data.Time.Clock (NominalDiffTime, addUTCTime)
import           Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import           Web.Scotty.Trans (ActionT, ScottyError)

type AppAction a = ActionT Text AppEnvSTM a

getTokenOrRaise :: AppAction Text
getTokenOrRaise = do
  mToken <- liftSTM $ fmap (^. E.accessToken) readAppEnv
  token  <- raiseIfNothing "Authorization token not found" mToken
  return $ token ^. T.accessToken

refreshTokenIfExpired :: AppAction ()
refreshTokenIfExpired = do
  mToken  <- liftSTM $ fmap (^. E.accessToken) readAppEnv
  expired <- liftIO $ maybe (return True) (tokenExpired 10) mToken
  when expired refreshToken

refreshToken :: AppAction ()
refreshToken = do
  conf <- liftSTM $ fmap (^. E.config) readAppEnv
  let params = AT.Params { AT._clientId     = conf ^. C.clientId
                         , AT._clientSecret = conf ^. C.clientSecret
                         , AT._tenantId     = conf ^. C.tenantId
                         }
  resp <- raiseLeft =<< liftIO (AT.acquireAccessToken params)
  liftSTM $ modifyAppEnv (& E.accessToken .~ Just (T.fromResponse resp))

tokenExpired :: NominalDiffTime -> T.AccessToken -> IO Bool
tokenExpired offset t = do
  now <- getSystemTime
  return $ t ^. T.expiresOn < addUTCTime (- offset) (systemToUTCTime now)
