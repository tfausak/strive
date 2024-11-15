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
import qualified Data.Semigroup as Semigroup
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums
  ( AgeGroup,
    Gender,
    SegmentActivityType (Riding),
    WeightClass,
  )
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getStarredSegments'
type GetStarredSegmentsOptions = PaginationOptions

-- | 'Strive.Actions.getSegmentEfforts'
data GetSegmentEffortsOptions = GetSegmentEffortsOptions
  { getSegmentEffortsOptions_athleteId :: Monoid.Last Integer,
    getSegmentEffortsOptions_range :: Monoid.Last (UTCTime, UTCTime),
    getSegmentEffortsOptions_page :: Semigroup.Last Integer,
    getSegmentEffortsOptions_perPage :: Semigroup.Last Integer
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
        getSegmentEffortsOptions_page = pure 1,
        getSegmentEffortsOptions_perPage = pure 200
      }

instance QueryLike GetSegmentEffortsOptions where
  toQuery options =
    toQuery
      [ ("athlete_id", fmap show (Monoid.getLast (getSegmentEffortsOptions_athleteId options))),
        ( "start_date_local",
          fmap
            (unpack . toStrict . encode . fst)
            (Monoid.getLast (getSegmentEffortsOptions_range options))
        ),
        ( "end_date_local",
          fmap
            (unpack . toStrict . encode . snd)
            (Monoid.getLast (getSegmentEffortsOptions_range options))
        ),
        ("page", Just (show (Semigroup.getLast (getSegmentEffortsOptions_page options)))),
        ("per_page", Just (show (Semigroup.getLast (getSegmentEffortsOptions_perPage options))))
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
    getSegmentLeaderboardOptions_page :: Semigroup.Last Integer,
    getSegmentLeaderboardOptions_perPage :: Semigroup.Last Integer
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
        getSegmentLeaderboardOptions_page = pure 1,
        getSegmentLeaderboardOptions_perPage = pure 200
      }

instance QueryLike GetSegmentLeaderboardOptions where
  toQuery options =
    toQuery
      [ ("gender", fmap show (Monoid.getLast (getSegmentLeaderboardOptions_gender options))),
        ("age_group", fmap show (Monoid.getLast (getSegmentLeaderboardOptions_ageGroup options))),
        ( "weight_class",
          fmap show (Monoid.getLast (getSegmentLeaderboardOptions_weightClass options))
        ),
        ( "following",
          fmap
            (unpack . toStrict . encode)
            (Monoid.getLast (getSegmentLeaderboardOptions_following options))
        ),
        ("club_id", fmap show (Monoid.getLast (getSegmentLeaderboardOptions_clubId options))),
        ("date_range", Monoid.getLast (getSegmentLeaderboardOptions_dateRange options)),
        ( "context_entries",
          fmap show (Monoid.getLast (getSegmentLeaderboardOptions_contextEntries options))
        ),
        ("page", Just (show (Semigroup.getLast (getSegmentLeaderboardOptions_page options)))),
        ("per_page", Just (show (Semigroup.getLast (getSegmentLeaderboardOptions_perPage options))))
      ]

-- | 'Strive.Actions.exploreSegments'
data ExploreSegmentsOptions = ExploreSegmentsOptions
  { exploreSegmentsOptions_activityType :: Semigroup.Last SegmentActivityType,
    exploreSegmentsOptions_minCat :: Semigroup.Last Integer,
    exploreSegmentsOptions_maxCat :: Semigroup.Last Integer
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
      { exploreSegmentsOptions_activityType = pure Riding,
        exploreSegmentsOptions_minCat = pure 0,
        exploreSegmentsOptions_maxCat = pure 5
      }

instance QueryLike ExploreSegmentsOptions where
  toQuery options =
    toQuery
      [ ("activity_type", show (Semigroup.getLast (exploreSegmentsOptions_activityType options))),
        ("min_cat", show (Semigroup.getLast (exploreSegmentsOptions_minCat options))),
        ("max_cat", show (Semigroup.getLast (exploreSegmentsOptions_maxCat options)))
      ]
