{-# LANGUAGE OverloadedStrings #-}

-- | Get resource price and meter metadata for a subscription.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)>
module Data.Billing.GetRateCardRequest
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
    { subscriptionId :: {-# UNPACK #-} !Text
    , offerId        :: {-# UNPACK #-} !Text
    , currency       :: {-# UNPACK #-} !Text
    , locale         :: {-# UNPACK #-} !Text
    , regionInfo     :: {-# UNPACK #-} !Text
    } deriving Show

-- | Construct URL from subscription ID.
url :: Text -> String
url subscriptionId' =
    "https://management.azure.com/subscriptions/"
    <> unpack subscriptionId'
    <> "/providers/Microsoft.Commerce/RateCard"

-- | Construct query string parameters from params.
--
--   <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
    [ ("api-version", Just $ encodeUtf8 billingApiVersion)
    , ("$filter",     Just $ encodeUtf8 $ filterQuery p)
    ]

-- | Construct $filter query from params.
filterQuery :: Params -> Text
filterQuery p =
    "OfferDurableId eq '"
    <> offerId p
    <> "' and Currency eq '"
    <> currency p
    <> "' and Locale eq '"
    <> locale p
    <> "' and RegionInfo eq '"
    <> regionInfo p
    <> "'"

-- | Construct request from access token and params.
--
--   The request has a modified response timeout of 180 seconds.
request :: Text -> Params -> Request
request token p =
    setQueryString params $ addAuthHeader token req'
  where
    params = queryParams p
    req    = parseRequest_ $ url $ subscriptionId p
    req'   = req { responseTimeout = responseTimeoutMicro 180000000 }
