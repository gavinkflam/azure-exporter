{-# LANGUAGE OverloadedStrings #-}

module Data.Dummy.Text
  (
  -- * Error
    errorCode
  , errorMessage
  , errorResponseJSON
  , errorValueJSON
  , oAuth2ErrorResponseJSON
  , oAuth2ErrorDescriptionLines
  -- * JSON
  , jsonValueJSON
  , jsonValueValue
  -- * Metrics
  , aggregation
  , metricNames
  -- * OAuth 2.0
  , accessToken
  , clientId
  , clientSecret
  , tenantId
  -- * Resource
  , resourceId
  , resourceGroup
  , resourceName
  , resourceProvider
  , resourceType
  , resourceRegion
  , subscriptionId
  , subResourceId
  -- * Timespan
  , timespan
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text, intercalate, pack)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)

-- | Dummy error code.
errorCode :: Text
errorCode = "InvalidOperation"

-- | Dummy error message.
errorMessage :: Text
errorMessage = "The system is going to explode!"

-- | Dummy `ErrorResponse` in JSON `ByteString`.
errorResponseJSON :: ByteString
errorResponseJSON = encodeUtf8 $
  "{" <>
    "\"error\":" <> decodeUtf8 errorValueJSON <>
  "}"

-- | Dummy `ErrorValue` in JSON `ByteString`.
errorValueJSON :: ByteString
errorValueJSON = encodeUtf8 $
  "{" <>
    "\"code\": \"" <> errorCode <> "\"," <>
    "\"message\": \"" <> errorMessage <> "\"" <>
  "}"

-- | Dummy `Azure.Data.OAuth2.ErrorResponse` in JSON ByteString.
oAuth2ErrorResponseJSON :: ByteString
oAuth2ErrorResponseJSON = encodeUtf8 $
  "{" <>
    "\"error\": \"interaction_required\"," <>
    "\"error_description\": \"" <>
      intercalate "\r\n" oAuth2ErrorDescriptionLines <> "\"," <>
    "\"error_codes\": [50079]," <>
    "\"timestamp\": \"2017-05-01 22:43:20Z\"," <>
    "\"trace_id\": \"b72a68c3-0926-4b8e-bc35-3150069c2800\"," <>
    "\"correlation_id\": \"73d656cf-54b1-4eb2-b429-26d8165a52d7\"," <>
    "\"claims\": \"Truncated\"" <>
  "}"

-- | Dummy error description lines for `oAuth2ErrorResponseJSON`.
oAuth2ErrorDescriptionLines :: [Text]
oAuth2ErrorDescriptionLines =
  [ "AADSTS50079: Truncated."
  , "Trace ID: b72a68c3-0926-4b8e-bc35-3150069c2800"
  , "Correlation ID: 73d656cf-54b1-4eb2-b429-26d8165a52d7"
  , "Timestamp: 2017-05-01 22:43:20Z"
  ]

-- | Dummy `JsonValue` in JSON `ByteString`.
jsonValueJSON :: ByteString
jsonValueJSON = encodeUtf8 $
  "{" <>
    "\"value\": " <> pack (show jsonValueValue) <>
  "}"

-- | Dummy inner value of `JsonValue` JSON `ByteString`.
jsonValueValue :: Int
jsonValueValue = 42

-- | Dummy aggregation types text.
aggregation :: Text
aggregation = "average,count"

-- | Dummy metric names text.
metricNames :: Text
metricNames = "Percentage CPU,Network In,Network Out"

-- | Dummy OAuth 2.0 access token.
accessToken :: Text
accessToken =
  "eyJhbGciOiJSUzI1NiIsIng1dCI6IjdkRC1nZWNOZ1gxWmY3R0xrT3ZwT0IyZGNWQSIsInR5cCI6IkpXVCJ9.eyJhdWQiOiJodHRwczovL3NlcnZpY2UuY29udG9zby5jb20vIiwiaXNzIjoiaHR0cHM6Ly9zdHMud2luZG93cy5uZXQvN2ZlODE0NDctZGE1Ny00Mzg1LWJlY2ItNmRlNTdmMjE0NzdlLyIsImlhdCI6MTM4ODQ0ODI2NywibmJmIjoxMzg4NDQ4MjY3LCJleHAiOjEzODg0NTIxNjcsInZlciI6IjEuMCIsInRpZCI6IjdmZTgxNDQ3LWRhNTctNDM4NS1iZWNiLTZkZTU3ZjIxNDc3ZSIsIm9pZCI6ImE5OTE5MTYyLTkyMTctNDlkYS1hZTIyLWYxMTM3YzI1Y2RlYSIsInN1YiI6ImE5OTE5MTYyLTkyMTctNDlkYS1hZTIyLWYxMTM3YzI1Y2RlYSIsImlkcCI6Imh0dHBzOi8vc3RzLndpbmRvd3MubmV0LzdmZTgxNDQ3LWRhNTctNDM4NS1iZWNiLTZkZTU3ZjIxNDc3ZS8iLCJhcHBpZCI6ImQxN2QxNWJjLWM1NzYtNDFlNS05MjdmLWRiNWYzMGRkNThmMSIsImFwcGlkYWNyIjoiMSJ9.aqtfJ7G37CpKV901Vm9sGiQhde0WMg6luYJR4wuNR2ffaQsVPPpKirM5rbc6o5CmW1OtmaAIdwDcL6i9ZT9ooIIicSRrjCYMYWHX08ip-tj-uWUihGztI02xKdWiycItpWiHxapQm0a8Ti1CWRjJghORC1B1-fah_yWx6Cjuf4QE8xJcu-ZHX0pVZNPX22PHYV5Km-vPTq2HtIqdboKyZy3Y4y3geOrRIFElZYoqjqSv5q9Jgtj5ERsNQIjefpyxW3EwPtFqMcDm4ebiAEpoEWRN4QYOMxnC9OUBeG9oLA0lTfmhgHLAtvJogJcYFzwngTsVo6HznsvPWy7UP3MINA"

-- | Dummy client ID.
clientId :: Text
clientId = "10d0e26a-e74e-4778-9d0b-5834de5a6956"

-- | Dummy client secret.
clientSecret :: Text
clientSecret = "aHR0cHM6Ly9naXRsYWIuY29tL2dhdmlua2ZsYW0vYXp1cmUtZXhwb3J0ZXI="

-- | Dummy tenant ID.
tenantId :: Text
tenantId = "ab2a7c40-80ea-4cd3-9ea1-10552c4df51d"

-- | Dummy resource URI.
resourceId :: Text
resourceId =
  "/subscriptions/" <> subscriptionId <>
  "/resourceGroups/" <> resourceGroup <>
  "/providers/" <> resourceProvider <>
  "/virtualMachines/" <> resourceName

-- | Resource group name for `resourceId`.
resourceGroup :: Text
resourceGroup = "DummyGroup"

-- | Resource name for `resourceId`.
resourceName :: Text
resourceName = "DummyVM"

-- | Resource provider for `resourceId`.
resourceProvider :: Text
resourceProvider = "Microsoft.Compute"

-- | Resource type for `resourceId`.
resourceType :: Text
resourceType = "virtualMachines"

-- | Resource region for fictional dummy resource.
resourceRegion :: Text
resourceRegion = "eastasia"

-- | Subscription ID for `resourceId`.
subscriptionId :: Text
subscriptionId = "312a4ad3-78e8-4b85-aa85-fdf7041f8155"

-- | Dummy sub-resource ID.
subResourceId :: Text
subResourceId = resourceId <> "/extensions/LinuxDiagnostic"

-- | Dummy timespan text.
timespan :: Text
timespan = "2018-10-08T09:01:10Z/2018-10-08T09:02:10Z"
