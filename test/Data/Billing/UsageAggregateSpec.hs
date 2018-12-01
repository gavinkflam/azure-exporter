{-# LANGUAGE OverloadedStrings #-}

-- | Test gauges deriving from `UsageAggregate`.
module Data.Billing.UsageAggregateSpec
    (
      -- * Spec
      spec
    ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeOrError)

import qualified Data.Billing.AggregateProperty as Ap
import qualified Data.Billing.GetRateCardResponse as Gr
import qualified Data.Billing.InstanceData as Id
import qualified Data.Billing.Meter as M
import qualified Data.Billing.ResourceData as Rd
import qualified Data.Billing.UsageAggregate as Ua
import Data.HashMap.Strict (HashMap, (!))
import qualified Data.HashMap.Strict as HM
import qualified Data.Prometheus.Gauge as G
import Data.Scientific (Scientific)
import Test.Hspec

-- | Spec for `UsageAggregate`.
spec :: Spec
spec =
    describe "toGauges" $
        it "derive the expected gauges from usage aggregates" $
            Ua.toGauges testRateCard testUsageAggregates
            `shouldBe` expectedGauges

-- | Test rate card for `toGauges` test.
testRateCard :: Gr.GetRateCardResponse
testRateCard = Gr.GetRateCardResponse
    { Gr._meters        = testMeters
    , Gr._currency      = testTexts ! "currency"
    , Gr._locale        = testTexts ! "locale"
    , Gr._isTaxIncluded = False
    }

-- | Test meters for `toGauges` test.
testMeters :: [M.Meter]
testMeters =
    [ M.Meter
        { M._meterId          = testTexts ! "m1Id"
        , M._meterName        = testTexts ! "m1Name"
        , M._meterCategory    = testTexts ! "m1Category"
        , M._meterSubCategory = testTexts ! "m1SubCategory"
        , M._meterRegion      = testTexts ! "m1Region"
        , M._meterRates       = m1Rates
        , M._unit             = testTexts ! "m1Unit"
        }
    ]

-- | Test meter rates for `toGauges` test.
m1Rates :: HashMap Text Scientific
m1Rates = HM.fromList
    [ ("0", read $ T.unpack $ testTexts ! "m1Rates0")
    ]

-- | Test usage aggregates for `toGauges` test.
testUsageAggregates :: [Ua.UsageAggregate]
testUsageAggregates =
    [ Ua.UsageAggregate
        { Ua.__id        = testTexts ! "ua1Id"
        , Ua._name       = testTexts ! "ua1Name"
        , Ua._properties = ua1Properties
        , Ua.__type      = testTexts ! "ua1Type"
        }
    ]

-- | Test usage properties for `toGauges` test.
ua1Properties :: Ap.AggregateProperty
ua1Properties = Ap.AggregateProperty
    { Ap._meterId          = testTexts ! "m1Id"
    , Ap._meterCategory    = Just $ testTexts ! "m1Category"
    , Ap._meterSubCategory = Just $ testTexts ! "m1SubCategory"
    , Ap._meterName        = Just $ testTexts ! "m1Name"
    , Ap._meterRegion      = Just $ testTexts ! "m1Region"
    , Ap._subscriptionId   = testTexts ! "subscriptionId"
    , Ap._unit             = Just $ testTexts ! "m1Unit"
    , Ap._usageStartTime   = up1StartTime
    , Ap._usageEndTime     = up1EndTime
    , Ap._instanceData     = Just up1InstanceData
    , Ap._quantity         = read $ T.unpack $ testTexts ! "up1Quantity"
    }

-- | Test time for `toGagues` test (Tuesday, 26-Jun-18 08:00:00 UTC).
up1StartTime :: UTCTime
up1StartTime = parseUTCTime $ testTexts ! "up1StartTime"

-- | Test time for `toGagues` test (Tuesday, 26-Jun-18 08:01:00 UTC).
up1EndTime :: UTCTime
up1EndTime = parseUTCTime $ testTexts ! "up1EndTime"

-- | Test instance data for `toGauges` test.
up1InstanceData :: Id.InstanceData
up1InstanceData = Id.InstanceData
    { Id._resourceData       = Rd.ResourceData
        { Rd._resourceUri    = testTexts ! "idt1ResourceUri"
        , Rd._location       = testTexts ! "idt1Location"
        , Rd._tags           = Just u1Tags
        , Rd._additionalInfo = Just u1AdditionalInfo
        }
    }

-- | Test tags for `toGauges` test.
u1Tags :: HashMap Text (Maybe Text)
u1Tags = HM.fromList
    [ (testTexts ! "tagAuthor",  Just $ testTexts ! "u1TagAuthor")
    , (testTexts ! "tagProject", Just $ testTexts ! "u1TagProject")
    ]

-- | Test additional info for `toGauges` test.
u1AdditionalInfo :: HashMap Text (Maybe Text)
u1AdditionalInfo = HM.fromList
    [ (testTexts ! "infoImageType",    Just $ testTexts ! "u1InfoImageType")
    , (testTexts ! "infoServiceType",  Just $ testTexts ! "u1InfoServiceType")
    , (testTexts ! "infoVMName",       Just $ testTexts ! "u1InfoVMName")
    , (testTexts ! "infoVMProperties", Just $ testTexts ! "u1InfoVMProperties")
    , (testTexts ! "infoUsageType",    Just $ testTexts ! "u1InfoUsageType")
    ]

-- | Expected gauges derived from `testResponse`.
expectedGauges :: [G.Gauge]
expectedGauges =
    [ G.Gauge
        { G._name   = (testTexts ! "g1NamePrefix") <> "_usage"
        , G._help   = (testTexts ! "g1NamePrefix") <> "_usage"
        , G._labels = g1Labels
        , G._value  = read $ T.unpack $ testTexts ! "up1Quantity"
        , G._time   = Just up1EndTime
        }
    , G.Gauge
        { G._name   = (testTexts ! "g1NamePrefix") <> "_cost"
        , G._help   = (testTexts ! "g1NamePrefix") <> "_cost"
        , G._labels = g1CostLabels
        , G._value  = read $ T.unpack $ testTexts ! "g1Cost"
        , G._time   = Just up1EndTime
        }
    ]

-- | Labels for expected gauges.
g1Labels :: [(Text, Text)]
g1Labels =
    [ ("cloud_provider",     "Azure")
    , ("meter_id",           testTexts ! "m1Id")
    , ("meter_category",     testTexts ! "m1Category")
    , ("meter_sub_category", testTexts ! "m1SubCategory")
    , ("meter_name",         testTexts ! "m1Name")
    , ("meter_region",       testTexts ! "m1Region")
    , ("meter_unit",         testTexts ! "m1Unit")
    , ("subscription_id",    testTexts ! "subscriptionId")
    -- Time labels
    , ("year",               testTexts ! "g1Year")
    , ("month",              testTexts ! "g1Month")
    , ("year_month",         testTexts ! "g1YearMonth")
    , ("date",               testTexts ! "g1Date")
    -- Instance data labels
    , ("resource_id",        T.toLower (testTexts ! "idt1ResourceUri"))
    , ("resource_region",    testTexts ! "idt1Location")
    , ("resource_group",     T.toLower (testTexts ! "idt1ResourceGroup"))
    , ("resource_name",      T.toLower (testTexts ! "idt1ResourceName"))
    , ("resource_provider",  T.toLower (testTexts ! "idt1ResourceProvider"))
    , ("resource_type",      T.toLower (testTexts ! "idt1ResourceType"))
    -- Tag labels
    , ("tag_Author",         testTexts ! "u1TagAuthor")
    , ("tag_Project",        testTexts ! "u1TagProject")
    , ("info_ServiceType",   testTexts ! "u1InfoServiceType")
    , ("info_ImageType",     testTexts ! "u1InfoImageType")
    , ("info_UsageType",     testTexts ! "u1InfoUsageType")
    ]

-- | Labels for expected gauges.
g1CostLabels :: [(Text, Text)]
g1CostLabels = g1Labels ++
    [ ("currency",  testTexts ! "currency")
    , ("unit_cost", testTexts ! "m1Rates0")
    ]

-- | Parse ISO8601 time with zone or error.  E.g. "2018-06-09T00:00:00+00:00".
parseUTCTime :: Text -> UTCTime
parseUTCTime =
    parseTimeOrError False defaultTimeLocale "%FT%H:%M:%S%z" . T.unpack

-- | Texts for test data.
testTexts :: HashMap Text Text
testTexts = HM.fromList
    [ ("currency",             "USD")
    , ("locale",               "en-US")
    , ("subscriptionId",       subscriptionId)
    -- Meter texts
    , ("m1Id",                 "12f637bb-97b1-43b4-9c56-5370b344d768")
    , ("m1Name",               "D1 v2/DS1 v2")
    , ("m1Category",           "Virtual Machines")
    , ("m1SubCategory",        "Dv2/DSv2 Series")
    , ("m1Region",             "CA East")
    , ("m1Unit",               "1 Hour")
    , ("m1Rates0",             "0.07")
    -- Usage texts
    , ("ua1Id",                usageIdPrefix <> ua1Name)
    , ("ua1Name",              ua1Name)
    , ("ua1Type",              "Microsoft.Commerce/UsageAggregate")
    , ("up1StartTime",         "2018-06-09T00:00:00+00:00")
    , ("up1EndTime",           "2018-06-10T00:00:00+00:00")
    , ("up1Quantity",          "1.616679")
    -- Instance data texts
    , ("idt1ResourceUri",      u1ResourceId)
    , ("idt1ResourceGroup",    u1ResourceGroup)
    , ("idt1ResourceName",     u1ResourceName)
    , ("idt1ResourceProvider", u1ResourceProvider)
    , ("idt1ResourceType",     u1ResourceType)
    , ("idt1Location",         "caeast")
    -- Tag texts
    , ("tagAuthor",            "Author")
    , ("tagProject",           "Project")
    , ("u1TagAuthor",          "linus")
    , ("u1TagProject",         "linux")
    -- Additional info texts
    , ("infoImageType",        "ImageType")
    , ("infoServiceType",      "ServiceType")
    , ("infoVMName",           "VMName")
    , ("infoVMProperties",     "VMProperties")
    , ("infoUsageType",        "UsageType")
    , ("u1InfoImageType",      "Canonical")
    , ("u1InfoServiceType",    "Standard_DS1_v2")
    , ("u1InfoVMName",         "")
    , ("u1InfoVMProperties",   "")
    , ("u1InfoUsageType",      "ComputeHR")
    -- Gauge
    , ("g1NamePrefix",         "azure_virtual_machines_d1_v2_ds1_v2_1_hour")
    , ("g1Cost",               "0.11316753")
    , ("g1Year",               "2018")
    , ("g1Month",              "06")
    , ("g1YearMonth",          "2018-06")
    , ("g1Date",               "2018-06-10")
    ]
  where
    ua1Name            = "Daily_BRSDT_20180601_0000"
    u1ResourceGroup    = "someGroup"
    u1ResourceName     = "blahblah"
    u1ResourceProvider = "Microsoft.Compute"
    u1ResourceType     = "virtualMachines"
    subscriptionId     = "312a4ad3-78e8-4b85-aa85-fdf7041f8155"
    usageIdPrefix      = T.concat
        ["/subscriptions/"
        , subscriptionId
        , "/providers/Microsoft.Commerce/UsageAggregates/"
        ]
    u1ResourceId       = T.concat
        [ "/subscriptions/" <> subscriptionId
        , "/resourceGroups/" <> u1ResourceGroup
        , "/providers/" <> u1ResourceProvider
        , "/" <> u1ResourceType <> "/" <> u1ResourceName
        ]
