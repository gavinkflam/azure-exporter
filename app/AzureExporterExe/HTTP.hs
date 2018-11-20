module AzureExporterExe.HTTP
  -- * Types
  ( IOResponse
  , STMResponse
  -- * Requests
  , requestIO
  , request
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString.Lazy (ByteString)

import           Control.Lens ((^.))
import           Data.Aeson (FromJSON)
import           Network.HTTP.Client (Manager, Request, httpLbs)

import           Data.Response.Aeson (ErrorHandler, errorExtractor, mapEitherDecode)
import           Control.Monad.AppEnvSTM
import qualified Data.AppEnv as E

-- |
-- Response comprising either `String` error messages or `FromJSON` response
-- with `IO` effect.
type IOResponse a = IO (Either String a)

-- |
-- Response comprising either `String` error messages or `FromJSON` response
-- with `AppEnvSTM` effect.
type STMResponse a = AppEnvSTM (Either String a)

-- |
-- Make a request with `IO` effect.
--
-- HTTP `Manager` and the error handler are required.
requestIO :: FromJSON a => Manager -> ErrorHandler -> Request -> IOResponse a
requestIO manager handler request = do
  res <- httpLbs request manager
  return $ mapEitherDecode handler res

-- |
-- Make a request with `AppEnvSTM` effect.
--
-- HTTP `Manager` will be obtained from the shared `AppEnv`.
--
-- `errorExtractor` will be applied as the error handler.
request :: FromJSON a => Request -> STMResponse a
request request = do
  manager <- fmap (^. E.httpManager) readAppEnv
  liftIO $ requestIO manager errorExtractor request
