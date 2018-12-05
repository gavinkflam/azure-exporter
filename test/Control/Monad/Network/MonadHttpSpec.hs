{-# LANGUAGE OverloadedStrings #-}

-- | Test HTTP request monad.
module Control.Monad.Network.MonadHttpSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS

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
    describe "runPure" $
        it "interprets the operations and return the expected response" $
            runPure testResponses testSeq `shouldBe` testResponseBody

-- | Test sequence to derive a text from environment variables.
testSeq :: MonadHttp m => m LBS.ByteString
testSeq = responseBody <$> httpLbs testRequest undefined

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
testResponses :: HashMap String (Response LBS.ByteString)
testResponses = HM.fromList
    [
        ( testPath
        , Response
            { responseStatus    = ok200
            , responseVersion   = http11
            , responseHeaders   = []
            , responseBody      = testResponseBody
            , responseCookieJar = mempty
            , responseClose'    = ResponseClose $ return ()
            }
        )
    ]
