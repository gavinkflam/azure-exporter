{-# LANGUAGE OverloadedStrings #-}

-- | Test request construction for the acquire access token API.
module Data.OAuth2.AcquireAccessTokenRequestSpec
    (
      -- * Spec
      spec
    ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.OAuth2.AcquireAccessTokenRequest as A
import Network.HTTP.Client (Request, RequestBody(..), path, requestBody)
import Network.HTTP.Types (parseSimpleQuery)
import Test.Hspec

import Test.Expectations (isJustOf)

-- | Spec for `AcquireAccessTokenRequest`.
spec :: Spec
spec = do
    let rPath  = path testRequest
        fItems = simpleRequestBody testRequest

    describe "request" $ do
        it "contains the expected grant type form item" $
            fItems `shouldContain` [queryItem "grant_type"]

        it "contains the expected resource form item" $
            fItems `shouldContain` [queryItem "resource"]

        it "contains the expected client id form item" $
            fItems `shouldContain` [queryItem "client_id"]

        it "contains the expected client secret form item" $
            fItems `shouldContain` [queryItem "client_secret"]

        it "contains the expected path" $
            rPath `shouldBe` expectedPath

    let err    = A.errorExtractor errorResponseJson
        nonErr = A.errorExtractor nonErrorResponseJson

    describe "errorExtractor" $ do
        it "extracts the first line of error descriptions" $
            err `shouldSatisfy` isJustOf (T.unpack $ head errorDescriptionLines)

        it "returns nothing for invalid error structure" $
            nonErr `shouldBe` Nothing

-- | Extract URL encoded request body into `SimpleQuery`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS` containing URL encoded content.
simpleRequestBody :: Request -> [(ByteString, ByteString)]
simpleRequestBody = parseSimpleQuery . toBS . requestBody

-- | Extract `ByteString` from `RequestBody`.
--
--   Caution: This function is defined only for `RequestBodyLBS` and
--   `RequestBodyBS`.
toBS :: RequestBody -> ByteString
toBS (RequestBodyLBS s) = LBS.toStrict s
toBS (RequestBodyBS  s) = s
toBS _                  = undefined :: ByteString

-- | Test request.
testRequest :: Request
testRequest = A.request testParams

-- | Test params.
testParams :: A.Params
testParams = A.Params
    { A.clientId     = testTexts ! "client_id"
    , A.clientSecret = testTexts ! "client_secret"
    , A.tenantId     = testTexts ! "tenant_id"
    }

-- | Derive the query item from `testTexts` key.
queryItem :: Text -> (ByteString, ByteString)
queryItem n = (encodeUtf8 n, encodeUtf8 (testTexts ! n))

-- | The expected path for `request`. It should:
--
-- #. Starts with the tenant ID
--
-- #. Follows by the API Endpoint
expectedPath :: ByteString
expectedPath = encodeUtf8 $ T.concat
    [ "/"
    , testTexts ! "tenant_id"
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

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("client_id",     "10d0e26a-e74e-4778-9d0b-5834de5a6956")
    , ("client_secret", "aHR0cHM6Ly9naXRsYWIuY29tL2dhdmlua2ZsYW0vYXp1cmUtZXhwb3J0ZXI=")
    , ("grant_type",    "client_credentials")
    , ("resource",      "https://management.azure.com/")
    , ("tenant_id",     "ab2a7c40-80ea-4cd3-9ea1-10552c4df51d")
    ]
