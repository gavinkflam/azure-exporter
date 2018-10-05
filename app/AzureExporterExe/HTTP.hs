module AzureExporterExe.HTTP
  ( request
  , request'
  ) where

import           Azure.Control.Error.Extractor (errorExtractor, mapEitherDecode)
import           AzureExporterExe.Control.Monad.AppEnvSTM
import qualified AzureExporterExe.Data.AppEnv as E
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import           Data.Text.Lazy (Text)
import           Network.HTTP.Client (Request, httpLbs, responseBody)

type Result a = AppEnvSTM (Either String a)

request :: FromJSON a => Request -> Result a
request = request' errorExtractor

request' :: (FromJSON a, FromJSON e) => (e -> Text) -> Request -> Result a
request' handler request = do
  manager <- fmap (^. E.httpManager) readAppEnv
  res     <- liftIO $ httpLbs request manager
  return $ mapEitherDecode handler $ responseBody res
