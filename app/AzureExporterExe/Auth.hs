{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Auth
  ( getTokenOrRaise
  ) where

import           AzureExporterExe.Control.Monad.AppEnvSTM
import           AzureExporterExe.Control.Monad.Maybe (raiseIfNothing)
import qualified AzureExporterExe.Data.AccessToken as T
import qualified AzureExporterExe.Data.AppEnv as E
import           Control.Lens ((^.))
import           Data.Text.Lazy (Text)
import           Web.Scotty.Trans (ActionT, ScottyError)

getTokenOrRaise :: ScottyError e => ActionT e AppEnvSTM Text
getTokenOrRaise = do
  mToken <- liftSTM $ fmap (^. E.accessToken) readAppEnv
  token  <- raiseIfNothing "Authorization token not found" mToken
  return $ token ^. T.accessToken
