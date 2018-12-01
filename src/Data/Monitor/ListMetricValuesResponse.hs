{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}

module Data.Monitor.ListMetricValuesResponse
    (
      -- * Types
      ListMetricValuesResponse (..)
      -- * Lenses
    , cost
    , interval
    , namespace
    , resourceregion
    , timespan
    , value
      -- * Gauges
    , toGauges
    ) where

import Data.Maybe (catMaybes)
import Data.Text (Text, intercalate, pack, unpack)
import GHC.Generics

import Control.Lens (makeLenses, (^.))
import Data.Aeson
import Data.Scientific (Scientific)
import Text.Casing (quietSnake)

import Data.Aeson.Options (aesonOptions)
import qualified Data.AzureRm.ResourceMetadata as D
import qualified Data.Monitor.LocalizableString as LS
import qualified Data.Monitor.Metric as M
import qualified Data.Monitor.MetricValue as V
import qualified Data.Monitor.TimeSeriesElement as E
import qualified Data.Prometheus.Gauge as G
import Text.AzureRm.Resource (parseResourceId, resourceId)

-- | Response for list metrics API.
--
--   <https://docs.microsoft.com/en-us/rest/api/monitor/metrics/list#response>
data ListMetricValuesResponse = ListMetricValuesResponse
    { _cost           :: {-# UNPACK #-} !Int
    , _interval       :: {-# UNPACK #-} !Text
    , _namespace      :: {-# UNPACK #-} !Text
    , _resourceregion :: {-# UNPACK #-} !Text
    , _timespan       :: {-# UNPACK #-} !Text
    , _value          :: [M.Metric]
    } deriving (Generic, Show)

instance FromJSON ListMetricValuesResponse where
    parseJSON = genericParseJSON aesonOptions

makeLenses ''ListMetricValuesResponse

-- | Construct gauges from `ListMetricValuesResponse`.
toGauges :: ListMetricValuesResponse -> [G.Gauge]
toGauges r =
    concatMap fGauges (r ^. value)
  where
    fGauges = metricToGauges (r ^. resourceregion)

-- | Construct gauges from `Metric`.
metricToGauges :: Text -> M.Metric -> [G.Gauge]
metricToGauges resourceRegion metric =
    concatMap fGauges values
  where
    metadata   = parseResourceId (metric ^. M._id)
    namePrefix = intercalate "_" $ map quietSnakeT
        [ "azure"
        , metadata ^. D.resourceType
        , metric ^. (M.name . LS.value)
        , metric ^. M.unit
        ]
    labels     = deriveLabels resourceRegion metadata metric
    fGauges    = metricValueToGauges namePrefix labels
    values     = concatMap (^. E._data) (metric ^. M.timeseries)

-- | Construct gauges from `MetricValue`.
metricValueToGauges :: Text -> [(Text, Text)] -> V.MetricValue -> [G.Gauge]
metricValueToGauges namePrefix labels metricValue = catMaybes
    [ fGauge "average" (metricValue ^. V.average)
    , fGauge "count"   (metricValue ^. V.count)
    , fGauge "maximum" (metricValue ^. V.maximum)
    , fGauge "minimum" (metricValue ^. V.minimum)
    , fGauge "total"   (metricValue ^. V.total)
    ]
  where
    fName  n = namePrefix <> "_" <> quietSnakeT n
    fGauge n = aggregationToGauges (fName n) labels

-- | Construct gauges from a individual aggregation.
aggregationToGauges
    :: Text -> [(Text, Text)] -> Maybe Scientific -> Maybe G.Gauge
aggregationToGauges _ _ Nothing          = Nothing
aggregationToGauges name labels (Just n) = Just G.Gauge
    { G._name   = name
    , G._help   = name
    , G._labels = labels
    , G._value  = n
    , G._time   = Nothing
    }

-- | Derive labels from `Metric`.
deriveLabels :: Text -> D.ResourceMetadata -> M.Metric -> [(Text, Text)]
deriveLabels resourceRegion metadata metric =
    [ ("resource_group",    metadata ^. D.resourceGroup)
    , ("resource_id",       resourceId (metric ^. M._id))
    , ("resource_name",     metadata ^. D.resourceName)
    , ("resource_provider", metadata ^. D.resourceProvider)
    , ("resource_region",   resourceRegion)
    , ("resource_type",     metadata ^. D.resourceType)
    , ("subscription_id",   metadata ^. D.subscriptionId)
    ]

-- | Convert text into underscore delimited lower case manner.
quietSnakeT :: Text -> Text
quietSnakeT = pack . quietSnake . unpack
