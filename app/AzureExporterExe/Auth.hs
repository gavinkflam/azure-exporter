{-# LANGUAGE OverloadedStrings #-}

module AzureExporterExe.Auth
  ( getTokenOrRaise
  ) where

import           AzureExporterExe.Control.Monad.AppEnvReader
import           AzureExporterExe.Control.Monad.Maybe (raiseIfNothing)
import qualified AzureExporterExe.Data.AccessToken as T
import qualified AzureExporterExe.Data.AppEnv as E
import           Control.Lens ((^.))
import           Data.Text.Lazy (Text)
import           Web.Scotty.Trans (ActionT, ScottyError)

getTokenOrRaise :: ScottyError e => ActionT e AppEnvReader Text
getTokenOrRaise = do
  mToken <- liftR $ getR $ (^. E.accessToken)
  token  <- raiseIfNothing "Authorization token not found" mToken
  return $ token ^. T.accessToken
