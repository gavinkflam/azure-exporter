{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lists the usage aggregates of a subscription.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)>
module Azure.Request.Billing.ListUsageAggregates
  (
  -- * Types
    Params (..)
  -- * Request
  , request
  ) where

import Azure.Contract (billingApiVersion)
import Azure.Util.HTTP (addAuthHeader)
import Control.Lens (makeLenses, (^.))
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Client (Request, parseRequest_, setQueryString)

-- | Parameters to construct `Request`.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
data Params =
  Params { _subscriptionId         :: Text
         , _aggregationGranularity :: Text
         , _reportedStartTime      :: Text
         , _reportedEndTime        :: Text
         , _continuationToken      :: Maybe Text
         } deriving Show

makeLenses ''Params

-- | Construct URL from subscription ID.
url :: Text -> String
url subscriptionId =
  "https://management.azure.com/subscriptions/"
  <> unpack subscriptionId
  <> "/providers/Microsoft.Commerce/UsageAggregates"

-- | Construct query string parameters from `Params`.
--
-- <https://docs.microsoft.com/en-us/previous-versions/azure/reference/mt219001(v%3dazure.100)#request>
queryParams :: Params -> [(ByteString, Maybe ByteString)]
queryParams p =
  [ ("api-version",            Just $ toStrict $ encodeUtf8 billingApiVersion)
  , ("aggregationGranularity", Just $ toStrict $ encodeUtf8 (p ^. aggregationGranularity))
  , ("reportedStartTime",      Just $ toStrict $ encodeUtf8 (p ^. reportedStartTime))
  , ("reportedEndTime",        Just $ toStrict $ encodeUtf8 (p ^. reportedEndTime))
  ]

-- | Construct `Request` from access token and `Params`.
request :: Text -> Params -> Request
request token p =
  setQueryString params $ addAuthHeader token req
    where params = queryParams p
          req    = parseRequest_ $ url (p ^. subscriptionId)
