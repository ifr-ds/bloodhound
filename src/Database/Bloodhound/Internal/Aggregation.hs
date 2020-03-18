{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Database.Bloodhound.Internal.Aggregation where

import           Bloodhound.Import

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Database.Bloodhound.Internal.Client
import           Database.Bloodhound.Internal.Highlight (HitHighlight)
import           Database.Bloodhound.Internal.Newtypes
import           Database.Bloodhound.Internal.Query
import           Database.Bloodhound.Internal.Sort

type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data Aggregation = TermsAgg TermsAggregation
                 | CardinalityAgg CardinalityAggregation
                 | RangeAgg RangeAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation
                 | DateRangeAgg DateRangeAggregation
                 | MissingAgg MissingAggregation
                 | TopHitsAgg TopHitsAggregation
                 | StatsAgg StatisticsAggregation
                 | CompositeAgg CompositeAggregation
                 | AverageAgg AverageAggregation
                 | PercentileAgg PercentileAggregation
  deriving (Eq, Show)

instance ToJSON Aggregation where
  toJSON (TermsAgg agg) = toJSON agg

  toJSON (CardinalityAgg (CardinalityAggregation field precisionThreshold)) =
    object ["cardinality" .= omitNulls [ "field"              .= field,
                                         "precisionThreshold" .= precisionThreshold
                                       ]
           ]

  toJSON (DateHistogramAgg
          (DateHistogramAggregation field interval format
           preZone postZone preOffset postOffset dateHistoAggs)) =
    omitNulls ["date_histogram" .= omitNulls [ "field"       .= field,
                                               "interval"    .= interval,
                                               "format"      .= format,
                                               "pre_zone"    .= preZone,
                                               "post_zone"   .= postZone,
                                               "pre_offset"  .= preOffset,
                                               "post_offset" .= postOffset
                                             ],
               "aggs"           .= dateHistoAggs ]
  toJSON (ValueCountAgg a) = object ["value_count" .= v]
    where v = case a of
                (FieldValueCount (FieldName n)) ->
                  object ["field" .= n]
                (ScriptValueCount s) ->
                  object ["script" .= s]
  toJSON (FilterAgg (FilterAggregation filt ags)) =
    omitNulls [ "filter" .= filt
              , "aggs" .= ags]
  toJSON (DateRangeAgg a) = object [ "date_range" .= a
                                   ]
  toJSON (MissingAgg (MissingAggregation{..})) =
    object ["missing" .= object ["field" .= maField]]

  toJSON (TopHitsAgg (TopHitsAggregation mfrom msize msort)) =
    omitNulls ["top_hits" .= omitNulls [ "size" .= msize
                                       , "from" .= mfrom
                                       , "sort" .= msort
                                       ]
              ]

  toJSON (StatsAgg (StatisticsAggregation typ field)) =
    object [stType .= omitNulls [ "field" .= field ]]
    where
      stType | typ == Basic = "stats"
             | otherwise = "extended_stats"

  toJSON (CompositeAgg CompositeAggregation{..}) =
    object [ "composite" .= omitNulls [ "sources" .= compositeSources
                                      , "size" .= compositeSize
                                      , "after" .= compositeAfter ] ]

  toJSON (AverageAgg agg) = toJSON agg
  toJSON (PercentileAgg agg) = toJSON agg
  toJSON (RangeAgg agg) = toJSON agg

data FieldOrScript = FieldValue Text | ScriptValue Text deriving (Eq, Show)

instance ToJSON FieldOrScript where
  toJSON (FieldValue f) = object [ "field" .= f ]
  toJSON (ScriptValue src) = object [ "script" .= object [ "source" .= src ]]

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From
  , taSize :: Maybe Size
  , taSort :: Maybe Sort
  } deriving (Eq, Show)

data MissingAggregation = MissingAggregation
  { maField :: Text
  } deriving (Eq, Show)

data TermsAggregation = TermsAggregation
  { term              :: Either Text Text
  , termInclude       :: Maybe TermInclusion
  , termExclude       :: Maybe TermInclusion
  , termOrder         :: Maybe TermOrder
  , termMinDocCount   :: Maybe Int
  , termSize          :: Maybe Int
  , termShardSize     :: Maybe Int
  , termCollectMode   :: Maybe CollectionMode
  , termExecutionHint :: Maybe ExecutionHint
  , termAggs          :: Maybe Aggregations
  } deriving (Eq, Show)

instance ToJSON TermsAggregation where
  toJSON (TermsAggregation term include exclude order minDocCount size shardSize collectMode executionHint termAggs) =
    omitNulls ["terms" .= omitNulls [ toJSON' term,
                                      "include"        .= include,
                                      "exclude"        .= exclude,
                                      "order"          .= order,
                                      "min_doc_count"  .= minDocCount,
                                      "size"           .= size,
                                      "shard_size"     .= shardSize,
                                      "collect_mode"   .= collectMode,
                                      "execution_hint" .= executionHint
                                    ],
               "aggs"  .= termAggs ]
    where
      toJSON' x = case x of { Left y -> "field" .= y;  Right y -> "script" .= y }

data CardinalityAggregation = CardinalityAggregation
  { cardinalityField   :: FieldName,
    precisionThreshold :: Maybe Int
  } deriving (Eq, Show)

data DateHistogramAggregation = DateHistogramAggregation
  { dateField      :: FieldName
  , dateInterval   :: Interval
  , dateFormat     :: Maybe Text
    -- pre and post deprecated in 1.5
  , datePreZone    :: Maybe Text
  , datePostZone   :: Maybe Text
  , datePreOffset  :: Maybe Text
  , datePostOffset :: Maybe Text
  , dateAggs       :: Maybe Aggregations
  } deriving (Eq, Show)

data DateRangeAggregation = DateRangeAggregation
  { draField  :: FieldName
  , draFormat :: Maybe Text
  , draRanges :: NonEmpty DateRangeAggRange
  } deriving (Eq, Show)

instance ToJSON DateRangeAggregation where
  toJSON DateRangeAggregation {..} =
    omitNulls [ "field" .= draField
              , "format" .= draFormat
              , "ranges" .= toList draRanges
              ]

data DateRangeAggRange =
    DateRangeFrom DateMathExpr
  | DateRangeTo DateMathExpr
  | DateRangeFromAndTo DateMathExpr DateMathExpr
  deriving (Eq, Show)

instance ToJSON DateRangeAggRange where
  toJSON (DateRangeFrom e)        = object [ "from" .= e ]
  toJSON (DateRangeTo e)          = object [ "to" .= e ]
  toJSON (DateRangeFromAndTo f t) = object [ "from" .= f, "to" .= t ]

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation =
    FieldValueCount FieldName
  | ScriptValueCount Script
  deriving (Eq, Show)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation
  { faFilter :: Filter
  , faAggs   :: Maybe Aggregations }
  deriving (Eq, Show)

data StatisticsAggregation = StatisticsAggregation
  { statsType :: StatsType
  , statsField :: FieldName }
  deriving (Eq, Show)

data StatsType
  = Basic
  | Extended
  deriving (Eq, Show)

data CompositeAggregation = CompositeAggregation
  { compositeSources :: [CompositeSource]
  , compositeAfter :: Maybe Value
  , compositeSize :: Maybe Int }
  deriving (Eq, Show)

data AverageAggregation = AverageAggregation
  { averageField :: FieldOrScript
  , averageMissing :: Maybe Value } deriving (Eq, Show)

instance ToJSON AverageAggregation where
  toJSON AverageAggregation{..} =
    omitNulls [ "avg" .= Object (toObject averageField <> missing) ]
    where
      missing = toObject . omitNulls $ [ "missing" .= averageMissing ]

data PercentileAggregation = PercentileAggregation
  { percentileField :: FieldOrScript
  , percentilePercents :: Maybe [Double] } deriving (Eq, Show)

instance ToJSON PercentileAggregation where
  toJSON PercentileAggregation{..} =
    omitNulls [ "percentiles" .= Object (toObject percentileField <> missing) ]
    where
      missing = toObject . omitNulls $ [ "percents" .= percentilePercents ]

data RangeAggregation = RangeAggregation
  { rangeField :: FieldOrScript
  , rangeRanges :: [RangeSpec] } deriving (Eq, Show)

instance ToJSON RangeAggregation where
  toJSON RangeAggregation{..} =
    omitNulls [ "range" .= Object (toObject rangeField <> ranges) ]
    where
      ranges = toObject . omitNulls $ [ "ranges" .= rangeRanges ]

data RangeSpec = RangeSpec
  { rangeSpecFrom :: Double
  , rangeSpecTo :: Double
  , rangeSpecKey :: Maybe Text } deriving (Eq, Show)

instance ToJSON RangeSpec where
  toJSON RangeSpec{..} = omitNulls [ "key" .= rangeSpecKey
                                   , "from" .= rangeSpecFrom
                                   , "to" .= rangeSpecTo ]

toObject :: ToJSON a => a -> Object
toObject a = case toJSON a of
  Object o -> o
  _        -> error "toObject: value isn't an Object"

-- TODO add other methods
data CompositeSource = CompositeTerms Text TermsAggregation deriving (Eq, Show)

instance ToJSON CompositeSource where
  toJSON (CompositeTerms name agg) = object [ name .= agg ]

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t =
  TermsAggregation (Left t)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

mkStatsAggregation :: FieldName -> StatisticsAggregation
mkStatsAggregation = StatisticsAggregation Basic

mkExtendedStatsAggregation :: FieldName -> StatisticsAggregation
mkExtendedStatsAggregation = StatisticsAggregation Extended

type AggregationResults = M.Map Text Value

class BucketAggregation a where
  key :: a -> BucketValue
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults

data Bucket a = Bucket
  { buckets :: [a]
  } deriving (Read, Show)


instance (FromJSON a) => FromJSON (Bucket a) where
  parseJSON (Object v) = Bucket <$>
                         v .: "buckets"
  parseJSON _ = mempty

data ScrollBucket a = ScrollBucket
  { scrollBuckets :: [a]
  , scrollAfterKey :: Maybe Value
  } deriving (Read, Show)

instance (FromJSON a) => FromJSON (ScrollBucket a) where
  parseJSON (Object v) = ScrollBucket <$>
                         v .: "buckets" <*>
                         v .:? "after_key"
  parseJSON _ = mempty


data BucketValue = TextValue Text
                 | ScientificValue Scientific
                 | BoolValue Bool deriving (Read, Show)

instance FromJSON BucketValue where
  parseJSON (String t) = return $ TextValue t
  parseJSON (Number s) = return $ ScientificValue s
  parseJSON (Bool b)   = return $ BoolValue b
  parseJSON _          = mempty

data TermInclusion = TermInclusion Text
                   | TermList [Text]
                   | TermPattern Text Text deriving (Eq, Show)

instance ToJSON TermInclusion where
  toJSON (TermInclusion x) = toJSON x
  toJSON (TermList x) = toJSON x
  toJSON (TermPattern pattern flags) =
    omitNulls [ "pattern" .= pattern
              , "flags"   .= flags]

data TermOrder = TermOrder
  { termSortField :: Text
  , termSortOrder :: SortOrder } deriving (Eq, Show)

instance ToJSON TermOrder where
  toJSON (TermOrder termSortField termSortOrder) =
    object [termSortField .= termSortOrder]

data CollectionMode = BreadthFirst
                    | DepthFirst deriving (Eq, Show)

instance ToJSON CollectionMode where
  toJSON BreadthFirst = "breadth_first"
  toJSON DepthFirst   = "depth_first"

data ExecutionHint = GlobalOrdinals
                   | Map deriving (Eq, Show)

instance ToJSON ExecutionHint where
  toJSON GlobalOrdinals               = "global_ordinals"
  toJSON Map                          = "map"

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr =
  DateMathExpr DateMathAnchor [DateMathModifier]
  deriving (Eq, Show)

instance ToJSON DateMathExpr where
  toJSON (DateMathExpr a mods) = String (fmtA a <> mconcat (fmtMod <$> mods))
    where fmtA DMNow         = "now"
          fmtA (DMDate date) = (T.pack $ showGregorian date) <> "||"
          fmtMod (AddTime n u)      = "+" <> showText n <> fmtU u
          fmtMod (SubtractTime n u) = "-" <> showText n <> fmtU u
          fmtMod (RoundDownTo u)    = "/" <> fmtU u
          fmtU DMYear   = "y"
          fmtU DMMonth  = "M"
          fmtU DMWeek   = "w"
          fmtU DMDay    = "d"
          fmtU DMHour   = "h"
          fmtU DMMinute = "m"
          fmtU DMSecond = "s"

-- | Starting point for a date range. This along with the 'DateMathModifiers' gets you the date ES will start from.
data DateMathAnchor =
    DMNow
  | DMDate Day
  deriving (Eq, Show)

data DateMathModifier =
    AddTime Int DateMathUnit
  | SubtractTime Int DateMathUnit
  | RoundDownTo DateMathUnit
  deriving (Eq, Show)

data DateMathUnit =
    DMYear
  | DMMonth
  | DMWeek
  | DMDay
  | DMHour
  | DMMinute
  | DMSecond
  deriving (Eq, Show)

data TermsResult = TermsResult
  { termKey       :: BucketValue
  , termsDocCount :: Int
  , termsAggs     :: Maybe AggregationResults
  } deriving (Read, Show)

instance FromJSON TermsResult where
  parseJSON (Object v) = TermsResult        <$>
                         v .:   "key"       <*>
                         v .:   "doc_count" <*>
                         (pure $ getNamedSubAgg v ["key", "doc_count"])
  parseJSON _ = mempty

instance BucketAggregation TermsResult where
  key = termKey
  docCount = termsDocCount
  aggs = termsAggs

data DateHistogramResult = DateHistogramResult
  { dateKey           :: Int
  , dateKeyStr        :: Maybe Text
  , dateDocCount      :: Int
  , dateHistogramAggs :: Maybe AggregationResults
  } deriving (Show)

instance FromJSON DateHistogramResult where
  parseJSON (Object v) = DateHistogramResult   <$>
                         v .:  "key"           <*>
                         v .:? "key_as_string" <*>
                         v .:  "doc_count"     <*>
                         (pure $ getNamedSubAgg v [ "key"
                                                  , "doc_count"
                                                  , "key_as_string"
                                                  ]
                         )
  parseJSON _ = mempty

instance BucketAggregation DateHistogramResult where
  key = TextValue . showText . dateKey
  docCount = dateDocCount
  aggs = dateHistogramAggs

data DateRangeResult = DateRangeResult
  { dateRangeKey          :: Text
  , dateRangeFrom         :: Maybe UTCTime
  , dateRangeFromAsString :: Maybe Text
  , dateRangeTo           :: Maybe UTCTime
  , dateRangeToAsString   :: Maybe Text
  , dateRangeDocCount     :: Int
  , dateRangeAggs         :: Maybe AggregationResults
  } deriving (Eq, Show)

instance FromJSON DateRangeResult where
  parseJSON = withObject "DateRangeResult" parse
    where parse v = DateRangeResult                 <$>
                    v .:  "key"                     <*>
                    (fmap posixMS <$> v .:? "from") <*>
                    v .:? "from_as_string"          <*>
                    (fmap posixMS <$> v .:? "to")   <*>
                    v .:? "to_as_string"            <*>
                    v .:  "doc_count"               <*>
                    (pure $ getNamedSubAgg v [ "key"
                                             , "from"
                                             , "from_as_string"
                                             , "to"
                                             , "to_as_string"
                                             , "doc_count"
                                             ]
                    )

instance BucketAggregation DateRangeResult where
  key = TextValue . dateRangeKey
  docCount = dateRangeDocCount
  aggs = dateRangeAggs

toTerms :: Text -> AggregationResults -> Maybe (Bucket TermsResult)
toTerms = toAggResult

toDateHistogram :: Text -> AggregationResults -> Maybe (Bucket DateHistogramResult)
toDateHistogram = toAggResult

toMissing :: Text -> AggregationResults -> Maybe MissingResult
toMissing = toAggResult

toTopHits :: (FromJSON a) => Text -> AggregationResults -> Maybe (TopHitResult a)
toTopHits = toAggResult

toAggResult :: (FromJSON a) => Text -> AggregationResults -> Maybe a
toAggResult t a = M.lookup t a >>= deserialize
  where deserialize = parseMaybe parseJSON

toCompositeResult :: (FromJSON a) => Text -> AggregationResults
                  -> Maybe (ScrollBucket (CompositeResult a))
toCompositeResult = toAggResult

newtype PercentileResult = PercentileResult { percentileResult :: HM.HashMap Text Double }
                         deriving (Show)

instance FromJSON PercentileResult where
  parseJSON = withObject "PercentileResult" $ \o -> PercentileResult <$> o .: "values"

toPercentileResult :: Text -> AggregationResults -> Maybe PercentileResult
toPercentileResult = toAggResult

-- Try to get an AggregationResults when we don't know the
-- field name. We filter out the known keys to try to minimize the noise.
getNamedSubAgg :: Object -> [Text] -> Maybe AggregationResults
getNamedSubAgg o knownKeys = maggRes
  where unknownKeys = HM.filterWithKey (\k _ -> k `notElem` knownKeys) o
        maggRes
          | HM.null unknownKeys = Nothing
          | otherwise           = Just . M.fromList $ HM.toList unknownKeys

data MetricResult = MetricResult { metricValue :: Double } deriving (Show)

instance FromJSON MetricResult where
  parseJSON = withObject "MetricResult" $ \o -> MetricResult <$> o .: "value"

data CompositeResult a = CompositeResult
  { compositeKey :: a
  , compositeDocCount :: Int } deriving (Show)

instance FromJSON a => FromJSON (CompositeResult a) where
  parseJSON = withObject "CompositeParse" parse
    where parse v = CompositeResult                 <$>
                    v .:  "key"                     <*>
                    v .:  "doc_count"

data MissingResult = MissingResult
  { missingDocCount :: Int
  } deriving (Show)

instance FromJSON MissingResult where
  parseJSON = withObject "MissingResult" parse
    where parse v = MissingResult <$> v .: "doc_count"

data TopHitResult a = TopHitResult
  { tarHits :: (SearchHits a)
  } deriving Show

instance (FromJSON a) => FromJSON (TopHitResult a) where
  parseJSON (Object v) = TopHitResult <$>
                         v .: "hits"
  parseJSON _          = fail "Failure in FromJSON (TopHitResult a)"

data HitsTotalRelation = HTR_EQ | HTR_GTE deriving (Eq, Show)

instance FromJSON HitsTotalRelation where
  parseJSON (String "eq")  = pure HTR_EQ
  parseJSON (String "gte") = pure HTR_GTE
  parseJSON _              = empty

data HitsTotal =
  HitsTotal { value    :: Int
            , relation :: HitsTotalRelation } deriving (Eq, Show)

instance FromJSON HitsTotal where
  parseJSON (Object v) = HitsTotal <$>
                         v .: "value"     <*>
                         v .: "relation"
  parseJSON _          = empty

instance Semigroup HitsTotal where
  (HitsTotal ta HTR_EQ)  <> (HitsTotal tb HTR_EQ)  = HitsTotal (ta + tb) HTR_EQ
  (HitsTotal ta HTR_GTE) <> (HitsTotal tb _)   = HitsTotal (ta + tb) HTR_GTE
  (HitsTotal ta _)       <> (HitsTotal tb HTR_GTE) = HitsTotal (ta + tb) HTR_GTE

data SearchHits a =
  SearchHits { hitsTotal :: HitsTotal
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)

instance (FromJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total"     <*>
                         v .: "max_score" <*>
                         v .: "hits"
  parseJSON _          = empty

instance Semigroup (SearchHits a) where
  (SearchHits ta ma ha) <> (SearchHits tb mb hb) =
    SearchHits (ta <> tb) (max ma mb) (ha <> hb)

instance Monoid (SearchHits a) where
  mempty = SearchHits (HitsTotal 0 HTR_EQ) Nothing mempty
  mappend = (<>)

type SearchAfterKey = [Aeson.Value]

data Hit a =
  Hit { hitIndex     :: IndexName
      , hitDocId     :: DocId
      , hitScore     :: Score
      , hitSource    :: Maybe a
      , hitSort      :: Maybe SearchAfterKey
      , hitFields    :: Maybe HitFields
      , hitHighlight :: Maybe HitHighlight } deriving (Eq, Show)

instance (FromJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) = Hit <$>
                         v .:  "_index"   <*>
                         v .:  "_id"      <*>
                         v .:  "_score"   <*>
                         v .:? "_source"  <*>
                         v .:? "sort"     <*>
                         v .:? "fields"   <*>
                         v .:? "highlight"
  parseJSON _          = empty
