module AzureExporterExe.HTTP
  -- * Types
  ( IOResponse
  , Response
  -- * Requests
  , requestIO
  , request
  ) where

import           Azure.Data.Aeson.Parser (errorExtractor, mapEitherDecode)
import           AzureExporterExe.Control.Monad.AppEnvSTM
import qualified AzureExporterExe.Data.AppEnv as E
import           Control.Lens ((^.))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (FromJSON)
import           Data.ByteString.Lazy (ByteString)
import           Network.HTTP.Client (Manager, Request, httpLbs, responseBody)

-- |
-- Response comprising either `String` error messages or `FromJSON` response
-- with `IO` effect.
type IOResponse a = IO (Either String a)

-- |
-- Response comprising either `String` error messages or `FromJSON` response
-- with `AppEnvSTM` effect.
type Response a   = AppEnvSTM (Either String a)

-- |
-- Make a request with `IO` effect.
--
-- HTTP `Manager` and the error handler are required.
requestIO
  :: FromJSON a
  => Manager -> (ByteString -> Maybe String) -> Request -> IOResponse a
requestIO manager handler request = do
  res <- httpLbs request manager
  return $ mapEitherDecode handler $ responseBody res

-- |
-- Make a request with `AppEnvSTM` effect.
--
-- HTTP `Manager` will be obtained from the shared `AppEnv`.
--
-- `errorExtractor` will be applied as the error handler.
request :: FromJSON a => Request -> Response a
request request = do
  manager <- fmap (^. E.httpManager) readAppEnv
  liftIO $ requestIO manager errorExtractor request
