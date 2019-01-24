{-# LANGUAGE OverloadedStrings #-}

-- | Lists the usage aggregates of a subscription.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)>
module Data.Billing.ListUsageAggregatesRequest
    (
      -- * Types
      Params (..)
      -- * Request
    , request
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
    (Request(..), parseRequest_, responseTimeoutMicro, setQueryString)

import Data.AzureRm.Contract (billingApiVersion)
import Data.AzureRm.Request (addAuthHeader)

-- | Parameters to construct request.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
data Params = Params
    { subscriptionId         :: {-# UNPACK #-} !Text
    , aggregationGranularity :: {-# UNPACK #-} !Text
    , reportedStartTime      :: {-# UNPACK #-} !Text
    , reportedEndTime        :: {-# UNPACK #-} !Text
    , continuationToken      :: Maybe Text
    } deriving Show

-- | Construct URL from subscription ID.
url :: Text -> String
url subscriptionId' =
    "https://management.azure.com/subscriptions/"
    <> unpack subscriptionId'
    <> "/providers/Microsoft.Commerce/UsageAggregates"

-- | Construct query string parameters from params.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
    maybeAdd params "continuationToken" maybeToken
  where
    maybeToken = encodeUtf8 <$> continuationToken p
    param      = Just . encodeUtf8
    params     =
        [ ("api-version",            param billingApiVersion)
        , ("aggregationGranularity", param $ aggregationGranularity p)
        , ("reportedStartTime",      param $ reportedStartTime p)
        , ("reportedEndTime",        param $ reportedEndTime p)
        ]

-- | Add the parameter to params for `Just` value, or return the original
--   params if `Nothing`.
maybeAdd
    :: [(ByteString, Maybe ByteString)]
    -> ByteString
    -> Maybe ByteString
    -> [(ByteString, Maybe ByteString)]
maybeAdd params name (Just v) = params ++ [(name, Just v)]
maybeAdd params _ _           = params

-- | Construct request from access token and params.
--
--   The request has a modified response timeout of 90 seconds.
request :: Text -> Params -> Request
request token p =
    setQueryString params $ addAuthHeader token req'
  where
    params = queryParams p
    req    = parseRequest_ $ url $ subscriptionId p
    req'   = req { responseTimeout = responseTimeoutMicro 90000000 }
