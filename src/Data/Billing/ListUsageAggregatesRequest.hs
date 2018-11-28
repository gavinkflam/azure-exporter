{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

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

import Control.Lens (makeLenses, (^.))

import Data.AzureRm.Contract (billingApiVersion)
import Data.AzureRm.Request (addAuthHeader)

-- | Parameters to construct `Request`.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
data Params = Params
    { _subscriptionId         :: {-# UNPACK #-} !Text
    , _aggregationGranularity :: {-# UNPACK #-} !Text
    , _reportedStartTime      :: {-# UNPACK #-} !Text
    , _reportedEndTime        :: {-# UNPACK #-} !Text
    , _continuationToken      :: Maybe Text
    } deriving Show

makeLenses ''Params

-- | Construct URL from subscription ID.
url :: Text -> String
url subscriptionId' =
    "https://management.azure.com/subscriptions/"
    <> unpack subscriptionId'
    <> "/providers/Microsoft.Commerce/UsageAggregates"

-- | Construct query string parameters from `Params`.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
    maybeAdd params "continuationToken" maybeToken
  where
    maybeToken = fmap encodeUtf8 (p ^. continuationToken)
    param      = Just . encodeUtf8
    params     =
        [ ("api-version",            param billingApiVersion)
        , ("aggregationGranularity", param (p ^. aggregationGranularity))
        , ("reportedStartTime",      param (p ^. reportedStartTime))
        , ("reportedEndTime",        param (p ^. reportedEndTime))
        ]

-- | Add the parameter to `Params` for `Just` value, or return the original
--   `Params` if `Nothing`.
maybeAdd
    :: [(ByteString, Maybe ByteString)]
    -> ByteString
    -> Maybe ByteString
    -> [(ByteString, Maybe ByteString)]
maybeAdd params name (Just v) = params ++ [(name, Just v)]
maybeAdd params _ _           = params

-- | Construct `Request` from access token and `Params`.
--
--   The `Request` has a modified response timeout of 90 seconds.
request :: Text -> Params -> Request
request token p =
    setQueryString params $ addAuthHeader token req'
  where
    params = queryParams p
    req    = parseRequest_ $ url (p ^. subscriptionId)
    req'   = req { responseTimeout = responseTimeoutMicro 90000000 }
