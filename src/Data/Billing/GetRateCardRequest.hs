{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Get resource price and meter metadata for a subscription.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219004(v%3dazure.100)>
module Data.Billing.GetRateCardRequest
  (
  -- * Types
    Params (..)
  -- * Request
  , request
  ) where

import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Control.Lens (makeLenses, (^.))
import Network.HTTP.Client (Request(..), parseRequest_, responseTimeoutMicro, setQueryString)

import Data.Contract (billingApiVersion)
import Text.HTTP (addAuthHeader)

-- | Parameters to construct `Request`.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
data Params =
  Params { _subscriptionId :: Text
         , _offerId        :: Text
         , _currency       :: Text
         , _locale         :: Text
         , _regionInfo     :: Text
         } deriving Show

makeLenses ''Params

-- | Construct URL from subscription ID.
url :: Text -> String
url subscriptionId =
  "https://management.azure.com/subscriptions/"
  <> unpack subscriptionId
  <> "/providers/Microsoft.Commerce/RateCard"

-- | Construct query string parameters from `Params`.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p = 
  [ ("api-version", Just $ toStrict $ encodeUtf8 billingApiVersion)
  , ("$filter",     Just $ toStrict $ encodeUtf8 $ filterQuery p)
  ]

-- | Construct $filter query from `Params`.
filterQuery :: Params -> Text
filterQuery p =
  "OfferDurableId eq '"
  <> p ^. offerId
  <> "' and Currency eq '"
  <> p ^. currency
  <> "' and Locale eq '"
  <> p ^. locale
  <> "' and RegionInfo eq '"
  <> p ^. regionInfo
  <> "'"

-- |
-- Construct `Request` from access token and `Params`.
--
-- The `Request` has a modified response timeout of 180 seconds.
request :: Text -> Params -> Request
request token p =
  setQueryString params $ addAuthHeader token req'
    where params = queryParams p
          req    = parseRequest_ $ url (p ^. subscriptionId)
          req'   = req { responseTimeout = responseTimeoutMicro 180000000 }
