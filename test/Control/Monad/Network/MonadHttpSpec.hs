{-# LANGUAGE OverloadedStrings #-}

-- | Test HTTP request monad.
module Control.Monad.Network.MonadHttpSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS

import Control.Exception (evaluate)
import Control.Monad (foldM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (Request, parseRequest_)
import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.Version (http11)
import Test.Hspec

import Control.Monad.Network.MonadHttp (MonadHttp, httpLbs, runPure)

-- | Spec for `MonadHttp`.
spec :: Spec
spec =
    describe "runPure" $ do
        it "returns the expected result" $
            runPure (testResponses 1) (testSeq 1) `shouldBe` testResponseBody
        it "returns the expected result for multiple requests" $
            runPure (testResponses 2) (testSeq 2) `shouldBe` testResponseBody
        it "throws no responses left error for too many requests" $
            evaluate seqEmpty `shouldThrow` errEmpty
        it "throws no responses matched error for unmatched request" $
            evaluate seqNoMatch `shouldThrow` errNoMatch
  where
    seqEmpty   = runPure (testResponses 2) (testSeq 3)
    seqNoMatch = runPure HM.empty (testSeq 1)
    errEmpty   = errorCall $ "no responses left for path " <> testPath
    errNoMatch = errorCall $ "no responses matched for path " <> testPath

-- | Test sequence to derive a text from environment variables.
testSeq :: MonadHttp m => Int -> m LBS.ByteString
testSeq n =
    foldM f "" $ replicate n testRequest
  where
    f _ req = responseBody <$> httpLbs req undefined

-- | Test request.
testRequest :: Request
testRequest = parseRequest_ testPath

-- | Path for `testRequest`.
testPath :: String
testPath = "https://example.com/test"

-- | Test response JSON text.
testResponseBody :: LBS.ByteString
testResponseBody = "{\"value\": 42}"

-- | Test responses.
testResponses :: Int -> HashMap String [Response LBS.ByteString]
testResponses n =
    HM.fromList [(testPath, replicate n response)]
  where
    response = Response
        { responseStatus    = ok200
        , responseVersion   = http11
        , responseHeaders   = []
        , responseBody      = testResponseBody
        , responseCookieJar = mempty
        , responseClose'    = ResponseClose $ return ()
        }
