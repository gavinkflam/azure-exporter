{-# LANGUAGE OverloadedStrings #-}

-- | Test HTTP request monad.
module Control.Monad.Network.HttpMSpec
    (
      -- * Spec
      spec
    ) where

import qualified Data.ByteString.Lazy as LBS

import Network.HTTP.Client.Internal (Response(..), ResponseClose(..))
import Network.HTTP.Types.Status (ok200)
import Network.HTTP.Types.Version (http11)
import Test.Hspec

import Control.Monad.Network.HttpM (HttpM, httpLbs, runPure)

-- | Spec for `HttpM`.
spec :: Spec
spec =
    describe "runPure" $
        it "interprets the operations and return the expected response" $
            runPure testResponse testSeq `shouldBe` testResponseBody

-- | Test sequence to derive a text from environment variables.
testSeq :: HttpM m => m LBS.ByteString
testSeq = responseBody <$> httpLbs undefined undefined

-- | Test response JSON text.
testResponseBody :: LBS.ByteString
testResponseBody = "{\"value\": 42}"

-- | Test response.
testResponse :: Response LBS.ByteString
testResponse = Response
    { responseStatus    = ok200
    , responseVersion   = http11
    , responseHeaders   = []
    , responseBody      = testResponseBody
    , responseCookieJar = mempty
    , responseClose'    = ResponseClose $ return ()
    }
