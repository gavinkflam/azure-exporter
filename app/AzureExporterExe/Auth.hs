{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Auth
  ( getTokenOrRaise
  , resolveToken
  ) where

import           AzureExporterExe.Control.Monad.AppEnvSTM
import           AzureExporterExe.Control.Monad.Either (raiseLeft)
import           AzureExporterExe.Control.Monad.Maybe (raiseIfNothing)
import qualified AzureExporterExe.Data.AccessToken as T
import qualified AzureExporterExe.Data.AppEnv as E
import qualified AzureExporterExe.Data.Config as C
import           Azure.Request.OAuth2.AcquireAccessToken as AT
import           Control.Lens ((^.), (&), (.~))
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

resolveToken :: AppAction Text
resolveToken = refreshTokenIfExpired >> getTokenOrRaise

refreshTokenIfExpired :: AppAction ()
refreshTokenIfExpired = do
  mToken    <- liftSTM $ fmap (^. E.accessToken) readAppEnv
  obsoleted <- liftIO $ tokenExpired 10 mToken
  if obsoleted then refreshToken else return ()

refreshToken :: AppAction ()
refreshToken = do
  token <- acquireToken
  liftSTM $ modifyAppEnv (& E.accessToken .~ Just token)

acquireToken :: AppAction T.AccessToken
acquireToken = do
  conf  <- liftSTM $ fmap (^. E.config) readAppEnv
  eResp <- liftIO $ AT.acquireAccessToken $ configToAuthParams conf
  resp  <- raiseLeft eResp
  return $ T.fromResponse resp

configToAuthParams :: C.Config -> AT.Params
configToAuthParams c =
  AT.Params { AT._clientId     = c ^. C.clientId
            , AT._clientSecret = c ^. C.clientSecret
            , AT._tenantId     = c ^. C.tenantId
            }

tokenExpired :: NominalDiffTime -> Maybe T.AccessToken -> IO Bool
tokenExpired _ Nothing       = return True
tokenExpired offset (Just t) = do
  now <- getSystemTime
  return $ t ^. T.expiresOn < (addUTCTime (- offset) $ systemToUTCTime now)
