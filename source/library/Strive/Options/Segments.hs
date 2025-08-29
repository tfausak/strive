-- | 'Strive.Actions.Segments'
module Strive.Options.Segments
  ( GetStarredSegmentsOptions,
    GetSegmentEffortsOptions (..),
    GetSegmentLeaderboardOptions (..),
    ExploreSegmentsOptions (..),
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Monoid as Monoid
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums
  ( AgeGroup,
    Gender,
    SegmentActivityType,
    WeightClass,
  )
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getStarredSegments'
type GetStarredSegmentsOptions = PaginationOptions

-- | 'Strive.Actions.getSegmentEfforts'
data GetSegmentEffortsOptions = GetSegmentEffortsOptions
  { getSegmentEffortsOptions_athleteId :: Monoid.Last Integer,
    getSegmentEffortsOptions_range :: Monoid.Last (UTCTime, UTCTime),
    getSegmentEffortsOptions_page :: Monoid.Last Integer,
    getSegmentEffortsOptions_perPage :: Monoid.Last Integer
  }
  deriving (Show)

instance Semigroup GetSegmentEffortsOptions where
  x <> y =
    GetSegmentEffortsOptions
      { getSegmentEffortsOptions_athleteId = getSegmentEffortsOptions_athleteId x <> getSegmentEffortsOptions_athleteId y,
        getSegmentEffortsOptions_range = getSegmentEffortsOptions_range x <> getSegmentEffortsOptions_range y,
        getSegmentEffortsOptions_page = getSegmentEffortsOptions_page x <> getSegmentEffortsOptions_page y,
        getSegmentEffortsOptions_perPage = getSegmentEffortsOptions_perPage x <> getSegmentEffortsOptions_perPage y
      }

instance Monoid GetSegmentEffortsOptions where
  mempty =
    GetSegmentEffortsOptions
      { getSegmentEffortsOptions_athleteId = mempty,
        getSegmentEffortsOptions_range = mempty,
        getSegmentEffortsOptions_page = mempty,
        getSegmentEffortsOptions_perPage = mempty
      }

instance QueryLike GetSegmentEffortsOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "athlete_id" . show) . Monoid.getLast $ getSegmentEffortsOptions_athleteId options,
        fmap ((,) "start_date_local" . unpack . toStrict . encode . fst) . Monoid.getLast $ getSegmentEffortsOptions_range options,
        fmap ((,) "end_date_local" . unpack . toStrict . encode . snd) . Monoid.getLast $ getSegmentEffortsOptions_range options,
        fmap ((,) "page" . show) . Monoid.getLast $ getSegmentEffortsOptions_page options,
        fmap ((,) "per_page" . show) . Monoid.getLast $ getSegmentEffortsOptions_perPage options
      ]

-- | 'Strive.Actions.getSegmentLeaderboard'
data GetSegmentLeaderboardOptions = GetSegmentLeaderboardOptions
  { getSegmentLeaderboardOptions_gender :: Monoid.Last Gender,
    getSegmentLeaderboardOptions_ageGroup :: Monoid.Last AgeGroup,
    getSegmentLeaderboardOptions_weightClass :: Monoid.Last WeightClass,
    getSegmentLeaderboardOptions_following :: Monoid.Last Bool,
    getSegmentLeaderboardOptions_clubId :: Monoid.Last Integer,
    getSegmentLeaderboardOptions_dateRange :: Monoid.Last String,
    getSegmentLeaderboardOptions_contextEntries :: Monoid.Last Integer,
    getSegmentLeaderboardOptions_page :: Monoid.Last Integer,
    getSegmentLeaderboardOptions_perPage :: Monoid.Last Integer
  }
  deriving (Show)

instance Semigroup GetSegmentLeaderboardOptions where
  x <> y =
    GetSegmentLeaderboardOptions
      { getSegmentLeaderboardOptions_gender = getSegmentLeaderboardOptions_gender x <> getSegmentLeaderboardOptions_gender y,
        getSegmentLeaderboardOptions_ageGroup = getSegmentLeaderboardOptions_ageGroup x <> getSegmentLeaderboardOptions_ageGroup y,
        getSegmentLeaderboardOptions_weightClass = getSegmentLeaderboardOptions_weightClass x <> getSegmentLeaderboardOptions_weightClass y,
        getSegmentLeaderboardOptions_following = getSegmentLeaderboardOptions_following x <> getSegmentLeaderboardOptions_following y,
        getSegmentLeaderboardOptions_clubId = getSegmentLeaderboardOptions_clubId x <> getSegmentLeaderboardOptions_clubId y,
        getSegmentLeaderboardOptions_dateRange = getSegmentLeaderboardOptions_dateRange x <> getSegmentLeaderboardOptions_dateRange y,
        getSegmentLeaderboardOptions_contextEntries = getSegmentLeaderboardOptions_contextEntries x <> getSegmentLeaderboardOptions_contextEntries y,
        getSegmentLeaderboardOptions_page = getSegmentLeaderboardOptions_page x <> getSegmentLeaderboardOptions_page y,
        getSegmentLeaderboardOptions_perPage = getSegmentLeaderboardOptions_perPage x <> getSegmentLeaderboardOptions_perPage y
      }

instance Monoid GetSegmentLeaderboardOptions where
  mempty =
    GetSegmentLeaderboardOptions
      { getSegmentLeaderboardOptions_gender = mempty,
        getSegmentLeaderboardOptions_ageGroup = mempty,
        getSegmentLeaderboardOptions_weightClass = mempty,
        getSegmentLeaderboardOptions_following = mempty,
        getSegmentLeaderboardOptions_clubId = mempty,
        getSegmentLeaderboardOptions_dateRange = mempty,
        getSegmentLeaderboardOptions_contextEntries = mempty,
        getSegmentLeaderboardOptions_page = mempty,
        getSegmentLeaderboardOptions_perPage = mempty
      }

instance QueryLike GetSegmentLeaderboardOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "gender" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_gender options,
        fmap ((,) "age_group" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_ageGroup options,
        fmap ((,) "weight_class" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_weightClass options,
        fmap ((,) "following" . unpack . toStrict . encode) . Monoid.getLast $ getSegmentLeaderboardOptions_following options,
        fmap ((,) "club_id" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_clubId options,
        fmap ((,) "date_range") . Monoid.getLast $ getSegmentLeaderboardOptions_dateRange options,
        fmap ((,) "context_entries" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_contextEntries options,
        fmap ((,) "page" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_page options,
        fmap ((,) "per_page" . show) . Monoid.getLast $ getSegmentLeaderboardOptions_perPage options
      ]

-- | 'Strive.Actions.exploreSegments'
data ExploreSegmentsOptions = ExploreSegmentsOptions
  { exploreSegmentsOptions_activityType :: Monoid.Last SegmentActivityType,
    exploreSegmentsOptions_minCat :: Monoid.Last Integer,
    exploreSegmentsOptions_maxCat :: Monoid.Last Integer
  }
  deriving (Show)

instance Semigroup ExploreSegmentsOptions where
  x <> y =
    ExploreSegmentsOptions
      { exploreSegmentsOptions_activityType = exploreSegmentsOptions_activityType x <> exploreSegmentsOptions_activityType y,
        exploreSegmentsOptions_minCat = exploreSegmentsOptions_minCat x <> exploreSegmentsOptions_minCat y,
        exploreSegmentsOptions_maxCat = exploreSegmentsOptions_maxCat x <> exploreSegmentsOptions_maxCat y
      }

instance Monoid ExploreSegmentsOptions where
  mempty =
    ExploreSegmentsOptions
      { exploreSegmentsOptions_activityType = mempty,
        exploreSegmentsOptions_minCat = mempty,
        exploreSegmentsOptions_maxCat = mempty
      }

instance QueryLike ExploreSegmentsOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "activity_type" . show) . Monoid.getLast $ exploreSegmentsOptions_activityType options,
        fmap ((,) "min_cat" . show) . Monoid.getLast $ exploreSegmentsOptions_minCat options,
        fmap ((,) "max_cat" . show) . Monoid.getLast $ exploreSegmentsOptions_maxCat options
      ]
