-- | 'Strive.Actions.Clubs'
module Strive.Options.Clubs
  ( GetClubMembersOptions
  , GetClubActivitiesOptions
  , GetStarredSegmentsOptions
  , GetSegmentEffortsOptions (..)
  , GetSegmentLeaderboardOptions (..)
  , ExploreSegmentsOptions (..)
  ) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (AgeGroup, Gender, SegmentActivityType (Riding),
                     WeightClass)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getClubMembers'
type GetClubMembersOptions = PaginationOptions

-- | 'Strive.Actions.getClubActivities'
type GetClubActivitiesOptions = PaginationOptions

-- | 'Strive.Actions.getStarredSegments'
type GetStarredSegmentsOptions = PaginationOptions

-- | 'Strive.Actions.getSegmentEfforts'
data GetSegmentEffortsOptions = GetSegmentEffortsOptions
  { getSegmentEffortsOptions_athleteId :: Maybe Integer
  , getSegmentEffortsOptions_range     :: Maybe (UTCTime, UTCTime)
  , getSegmentEffortsOptions_page      :: Integer
  , getSegmentEffortsOptions_perPage   :: Integer
  } deriving Show

instance Default GetSegmentEffortsOptions where
  def = GetSegmentEffortsOptions
    { getSegmentEffortsOptions_athleteId = Nothing
    , getSegmentEffortsOptions_range = Nothing
    , getSegmentEffortsOptions_page = 1
    , getSegmentEffortsOptions_perPage = 200
    }

instance QueryLike GetSegmentEffortsOptions where
  toQuery options = toQuery
    [ ("athlete_id", fmap show (getSegmentEffortsOptions_athleteId options))
    , ("start_date_local", fmap (unpack . toStrict . encode . fst) (getSegmentEffortsOptions_range options))
    , ("end_date_local", fmap (unpack . toStrict . encode . snd) (getSegmentEffortsOptions_range options))
    , ("page", Just (show (getSegmentEffortsOptions_page options)))
    , ("per_page", Just (show (getSegmentEffortsOptions_perPage options)))
    ]

-- | 'Strive.Actions.getSegmentLeaderboard'
data GetSegmentLeaderboardOptions = GetSegmentLeaderboardOptions
  { getSegmentLeaderboardOptions_gender      :: Maybe Gender
  , getSegmentLeaderboardOptions_ageGroup    :: Maybe AgeGroup
  , getSegmentLeaderboardOptions_weightClass :: Maybe WeightClass
  , getSegmentLeaderboardOptions_following   :: Maybe Bool
  , getSegmentLeaderboardOptions_clubId      :: Maybe Integer
  , getSegmentLeaderboardOptions_dateRange   :: Maybe String
  , getSegmentLeaderboardOptions_page        :: Integer
  , getSegmentLeaderboardOptions_perPage     :: Integer
  } deriving Show

instance Default GetSegmentLeaderboardOptions where
  def = GetSegmentLeaderboardOptions
    { getSegmentLeaderboardOptions_gender = Nothing
    , getSegmentLeaderboardOptions_ageGroup = Nothing
    , getSegmentLeaderboardOptions_weightClass = Nothing
    , getSegmentLeaderboardOptions_following = Nothing
    , getSegmentLeaderboardOptions_clubId = Nothing
    , getSegmentLeaderboardOptions_dateRange = Nothing
    , getSegmentLeaderboardOptions_page = 1
    , getSegmentLeaderboardOptions_perPage = 200
    }

instance QueryLike GetSegmentLeaderboardOptions where
  toQuery options = toQuery
    [ ("gender", fmap show (getSegmentLeaderboardOptions_gender options))
    , ("age_group", fmap show (getSegmentLeaderboardOptions_ageGroup options))
    , ("weight_class", fmap show (getSegmentLeaderboardOptions_weightClass options))
    , ("following", fmap (unpack . toStrict . encode) (getSegmentLeaderboardOptions_following options))
    , ("club_id", fmap show (getSegmentLeaderboardOptions_clubId options))
    , ("date_range", getSegmentLeaderboardOptions_dateRange options)
    , ("page", Just (show (getSegmentLeaderboardOptions_page options)))
    , ("per_page", Just (show (getSegmentLeaderboardOptions_perPage options)))
    ]

-- | 'Strive.Actions.exploreSegments'
data ExploreSegmentsOptions = ExploreSegmentsOptions
  { exploreSegmentsOptions_activityType :: SegmentActivityType
  , exploreSegmentsOptions_minCat       :: Integer
  , exploreSegmentsOptions_maxCat       :: Integer
  } deriving Show

instance Default ExploreSegmentsOptions where
  def = ExploreSegmentsOptions
    { exploreSegmentsOptions_activityType = Riding
    , exploreSegmentsOptions_minCat = 0
    , exploreSegmentsOptions_maxCat = 5
    }

instance QueryLike ExploreSegmentsOptions where
  toQuery options = toQuery
    [ ("activity_type", show (exploreSegmentsOptions_activityType options))
    , ("min_cat", show (exploreSegmentsOptions_minCat options))
    , ("max_cat", show (exploreSegmentsOptions_maxCat options))
    ]
