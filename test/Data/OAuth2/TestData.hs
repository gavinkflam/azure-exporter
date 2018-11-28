{-# LANGUAGE OverloadedStrings #-}

module Data.OAuth2.TestData
    (
      -- * Test data
      acquireAccessTokenRequest
      -- * Result
    , queryItem
    , expectedAcquireAccessTokenPath
        -- * Error result
    , errorResponseJson
    , errorDescriptionLines
    , nonErrorResponseJson
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.OAuth2.AcquireAccessTokenRequest as A
import Network.HTTP.Client (Request)

-- | Test request for `AcquireAccessTokenRequest`.
acquireAccessTokenRequest :: Request
acquireAccessTokenRequest = A.request acquireAccessTokenParams

-- | Test `Params` for `acquireAccessTokenRequest`.
acquireAccessTokenParams :: A.Params
acquireAccessTokenParams = A.Params
    { A._clientId     = testDatastore ! "client_id"
    , A._clientSecret = testDatastore ! "client_secret"
    , A._tenantId     = testDatastore ! "tenant_id"
    }

-- | Derive the query item from `testDatastore` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testDatastore ! n))

-- | The expected path for `acquireAccessTokenRequest`. It should:
--
-- #. Starts with the tenant ID
--
-- #. Follows by the API Endpoint
expectedAcquireAccessTokenPath :: ByteString
expectedAcquireAccessTokenPath = encodeUtf8 $ T.concat
    [ "/"
    , testDatastore ! "tenant_id"
    , "/oauth2/token"
    ]

-- | Test `ErrorResponse` JSON text.
errorResponseJson :: LBS.ByteString
errorResponseJson = LBS.fromStrict $ encodeUtf8 $ T.concat
    [ "{\"error\": \"interaction_required\""
    , ",\"error_description\": \""
    , T.intercalate "\r\n" errorDescriptionLines
    , "\""
    , ",\"error_codes\": [50079]"
    , ",\"timestamp\": \"2017-05-01 22:43:20Z\""
    , ",\"trace_id\": \"b72a68c3-0926-4b8e-bc35-3150069c2800\""
    , ",\"correlation_id\": \"73d656cf-54b1-4eb2-b429-26d8165a52d7\""
    , ",\"claims\": \"Truncated\""
    , "}"
    ]
-- | Error description lines for `errorResponseJson`.
errorDescriptionLines :: [Text]
errorDescriptionLines =
    [ "AADSTS50079: Truncated."
    , "Trace ID: b72a68c3-0926-4b8e-bc35-3150069c2800"
    , "Correlation ID: 73d656cf-54b1-4eb2-b429-26d8165a52d7"
    , "Timestamp: 2017-05-01 22:43:20Z"
    ]

-- | Test JSON text for non-error response.
nonErrorResponseJson :: LBS.ByteString
nonErrorResponseJson = "{\"value\": 42}"

-- | Datastore for test data.
testDatastore :: HashMap Text Text
testDatastore = HM.fromList
    [ ("client_id",     "10d0e26a-e74e-4778-9d0b-5834de5a6956")
    , ("client_secret", "aHR0cHM6Ly9naXRsYWIuY29tL2dhdmlua2ZsYW0vYXp1cmUtZXhwb3J0ZXI=")
    , ("grant_type",    "client_credentials")
    , ("resource",      "https://management.azure.com/")
    , ("tenant_id",     "ab2a7c40-80ea-4cd3-9ea1-10552c4df51d")
    ]
