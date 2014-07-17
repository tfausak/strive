-- | Optional parameters for actions.
module Strive.Options where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)
import Strive.Enums

data PaginationOptions = PaginationOptions
  { paginationOptions_page    :: Integer
  , paginationOptions_perPage :: Integer
  } deriving Show

instance Default PaginationOptions where
  def = PaginationOptions
    { paginationOptions_page = 1
    , paginationOptions_perPage = 200
    }

instance QueryLike PaginationOptions where
  toQuery options = toQuery
    [ ("page", show (paginationOptions_page options))
    , ("per_page", show (paginationOptions_perPage options))
    ]

-- * Authentication

-- | 'Strive.Actions.buildAuthorizeUrl'
data BuildAuthorizeUrlOptions = BuildAuthorizeUrlOptions
  { buildAuthorizeUrlOptions_approvalPrompt :: Bool
  , buildAuthorizeUrlOptions_privateScope   :: Bool
  , buildAuthorizeUrlOptions_writeScope     :: Bool
  , buildAuthorizeUrlOptions_state          :: String
  } deriving Show

instance Default BuildAuthorizeUrlOptions where
  def = BuildAuthorizeUrlOptions
    { buildAuthorizeUrlOptions_approvalPrompt = False
    , buildAuthorizeUrlOptions_privateScope = False
    , buildAuthorizeUrlOptions_writeScope = False
    , buildAuthorizeUrlOptions_state = ""
    }

instance QueryLike BuildAuthorizeUrlOptions where
  toQuery options = toQuery
    [ ("approval_prompt", unpack (toStrict (encode (buildAuthorizeUrlOptions_approvalPrompt options))))
    , ("scope", scopes)
    , ("state", buildAuthorizeUrlOptions_state options)
    ]
   where
    scopes = unwords
      [ if buildAuthorizeUrlOptions_privateScope options then "view_private" else ""
      , if buildAuthorizeUrlOptions_writeScope options then "write" else ""
      ]

-- * Athletes

-- | 'Strive.Actions.updateCurrentAthlete'
data UpdateCurrentAthleteOptions = UpdateCurrentAthleteOptions
  { updateCurrentAthleteOptions_city    :: Maybe String
  , updateCurrentAthleteOptions_state   :: Maybe String
  , updateCurrentAthleteOptions_country :: Maybe String
  , updateCurrentAthleteOptions_sex     :: Maybe Gender
  , updateCurrentAthleteOptions_weight  :: Maybe Double
  } deriving Show

instance Default UpdateCurrentAthleteOptions where
  def = UpdateCurrentAthleteOptions
    { updateCurrentAthleteOptions_city = Nothing
    , updateCurrentAthleteOptions_state = Nothing
    , updateCurrentAthleteOptions_country = Nothing
    , updateCurrentAthleteOptions_sex = Nothing
    , updateCurrentAthleteOptions_weight = Nothing
    }

instance QueryLike UpdateCurrentAthleteOptions where
  toQuery options = toQuery
    [ ("city", updateCurrentAthleteOptions_city options)
    , ("state", updateCurrentAthleteOptions_state options)
    , ("country", updateCurrentAthleteOptions_country options)
    , ("sex", fmap show (updateCurrentAthleteOptions_sex options))
    , ("weight", fmap show (updateCurrentAthleteOptions_weight options))
    ]

-- | 'Strive.Actions.getAthleteCrs'
type GetAthleteCrsOptions = PaginationOptions

-- * Friends and Followers

-- | 'Strive.Actions.getCurrentFriends'
type GetCurrentFriendsOptions = PaginationOptions

-- | 'Strive.Actions.getFriends'
type GetFriendsOptions = PaginationOptions

-- | 'Strive.Actions.getCurrentFollowers'
type GetCurrentFollowersOptions = PaginationOptions

-- | 'Strive.Actions.getFollowers'
type GetFollowersOptions = PaginationOptions

-- | 'Strive.Actions.getCommonFriends'
type GetCommonFriendsOptions = PaginationOptions

-- * Activities

-- | 'Strive.Actions.CreateActivity'
data CreateActivityOptions = CreateActivityOptions
  { createActivityOptions_description :: Maybe String
  , createActivityOptions_distance    :: Maybe Double
  } deriving Show

instance Default CreateActivityOptions where
  def = CreateActivityOptions
    { createActivityOptions_description = Nothing
    , createActivityOptions_distance = Nothing
    }

instance QueryLike CreateActivityOptions where
  toQuery options = toQuery
    [ ("description", createActivityOptions_description options)
    , ("distance", fmap show (createActivityOptions_distance options))
    ]

-- | 'Strive.Actions.GetActivity'
data GetActivityOptions = GetActivityOptions
  { getActivityOptions_allEfforts :: Bool
  } deriving Show

instance Default GetActivityOptions where
  def = GetActivityOptions
    { getActivityOptions_allEfforts = False
    }

instance QueryLike GetActivityOptions where
  toQuery options = toQuery
    [ ("approval_prompt", unpack (toStrict (encode (getActivityOptions_allEfforts options))))
    ]

-- | 'Strive.Actions.UpdateActivity'
data UpdateActivityOptions = UpdateActivityOptions
  { updateActivityOptions_name        :: Maybe String
  , updateActivityOptions_type        :: Maybe ActivityType
  , updateActivityOptions_private     :: Maybe Bool
  , updateActivityOptions_commute     :: Maybe Bool
  , updateActivityOptions_trainer     :: Maybe Bool
  , updateActivityOptions_gearId      :: Maybe String
  , updateActivityOptions_description :: Maybe String
  } deriving Show

instance Default UpdateActivityOptions where
  def = UpdateActivityOptions
    { updateActivityOptions_name = Nothing
    , updateActivityOptions_type = Nothing
    , updateActivityOptions_private = Nothing
    , updateActivityOptions_commute = Nothing
    , updateActivityOptions_trainer = Nothing
    , updateActivityOptions_gearId = Nothing
    , updateActivityOptions_description = Nothing
    }

instance QueryLike UpdateActivityOptions where
  toQuery options = toQuery
    [ ("name", updateActivityOptions_name options)
    , ("type", fmap show (updateActivityOptions_type options))
    , ("private", fmap (unpack . toStrict . encode) (updateActivityOptions_private options))
    , ("commute", fmap (unpack . toStrict . encode) (updateActivityOptions_commute options))
    , ("trainer", fmap (unpack . toStrict . encode) (updateActivityOptions_trainer options))
    , ("gear_id", updateActivityOptions_gearId options)
    , ("description", updateActivityOptions_description options)
    ]

-- | 'Strive.Actions.getCurrentActivities'
data GetCurrentActivitiesOptions = GetCurrentActivitiesOptions
  { getCurrentActivitiesOptions_before  :: Maybe UTCTime
  , getCurrentActivitiesOptions_after   :: Maybe UTCTime
  , getCurrentActivitiesOptions_page    :: Integer
  , getCurrentActivitiesOptions_perPage :: Integer
  } deriving Show

instance Default GetCurrentActivitiesOptions where
  def = GetCurrentActivitiesOptions
    { getCurrentActivitiesOptions_before = Nothing
    , getCurrentActivitiesOptions_after = Nothing
    , getCurrentActivitiesOptions_page = 1
    , getCurrentActivitiesOptions_perPage = 200
    }

instance QueryLike GetCurrentActivitiesOptions where
  toQuery options = toQuery
    [ ("before", fmap (show . utcTimeToPOSIXSeconds) (getCurrentActivitiesOptions_before options))
    , ("after", fmap (show . utcTimeToPOSIXSeconds) (getCurrentActivitiesOptions_after options))
    , ("page", Just (show (getCurrentActivitiesOptions_page options)))
    , ("per_page", Just (show (getCurrentActivitiesOptions_perPage options)))
    ]

-- | 'Strive.Actions.getFeed'
type GetFeedOptions = PaginationOptions

-- * Comments

-- | 'Strive.Actions.getActivityComments'
data GetActivityCommentsOptions = GetActivityCommentsOptions
  { getActivityCommentsOptions_markdown :: Bool
  , getActivityCommentsOptions_page     :: Integer
  , getActivityCommentsOptions_perPage  :: Integer
  } deriving Show

instance Default GetActivityCommentsOptions where
  def = GetActivityCommentsOptions
    { getActivityCommentsOptions_markdown = False
    , getActivityCommentsOptions_page = 1
    , getActivityCommentsOptions_perPage = 200
    }

instance QueryLike GetActivityCommentsOptions where
  toQuery options = toQuery
    [ ("before", unpack (toStrict (encode (getActivityCommentsOptions_markdown options))))
    , ("page", show (getActivityCommentsOptions_page options))
    , ("per_page", show (getActivityCommentsOptions_perPage options))
    ]

-- * Kudos

-- | 'Strive.Actions.getActivityKudoers'
type GetActivityKudoersOptions = PaginationOptions

-- * Clubs

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
  { getSegmentLeaderboard_gender      :: Maybe Gender
  , getSegmentLeaderboard_ageGroup    :: Maybe AgeGroup
  , getSegmentLeaderboard_weightClass :: Maybe String
  , getSegmentLeaderboard_following   :: Maybe Bool
  , getSegmentLeaderboard_clubId      :: Maybe Integer
  , getSegmentLeaderboard_dateRange   :: Maybe String
  , getSegmentLeaderboard_page        :: Integer
  , getSegmentLeaderboard_perPage     :: Integer
  } deriving Show

instance Default GetSegmentLeaderboardOptions where
  def = GetSegmentLeaderboardOptions
    { getSegmentLeaderboard_gender = Nothing
    , getSegmentLeaderboard_ageGroup = Nothing
    , getSegmentLeaderboard_weightClass = Nothing
    , getSegmentLeaderboard_following = Nothing
    , getSegmentLeaderboard_clubId = Nothing
    , getSegmentLeaderboard_dateRange = Nothing
    , getSegmentLeaderboard_page = 1
    , getSegmentLeaderboard_perPage = 200
    }

instance QueryLike GetSegmentLeaderboardOptions where
  toQuery options = toQuery
    [ ("gender", fmap show (getSegmentLeaderboard_gender options))
    , ("age_group", fmap show (getSegmentLeaderboard_ageGroup options))
    , ("weight_class", getSegmentLeaderboard_weightClass options)
    , ("following", fmap (unpack . toStrict . encode) (getSegmentLeaderboard_following options))
    , ("club_id", fmap show (getSegmentLeaderboard_clubId options))
    , ("date_range", getSegmentLeaderboard_dateRange options)
    , ("page", Just (show (getSegmentLeaderboard_page options)))
    , ("per_page", Just (show (getSegmentLeaderboard_perPage options)))
    ]

-- | 'Strive.Actions.exploreSegments'
data ExploreSegmentsOptions = ExploreSegmentsOptions
  { exploreSegmentsOptions_activityType :: String
  , exploreSegmentsOptions_minCat       :: Integer
  , exploreSegmentsOptions_maxCat       :: Integer
  } deriving Show

instance Default ExploreSegmentsOptions where
  def = ExploreSegmentsOptions
    { exploreSegmentsOptions_activityType = "riding"
    , exploreSegmentsOptions_minCat = 0
    , exploreSegmentsOptions_maxCat = 5
    }

instance QueryLike ExploreSegmentsOptions where
  toQuery options = toQuery
    [ ("activity_type", exploreSegmentsOptions_activityType options)
    , ("min_cat", show (exploreSegmentsOptions_minCat options))
    , ("max_cat", show (exploreSegmentsOptions_maxCat options))
    ]

-- * Streams

-- | 'Strive.Actions.getStreams'
data GetStreamsOptions = GetStreamsOptions
  { getStreamsOptions_resolution :: Maybe String
  , getStreamsOptions_seriesType :: String
  } deriving Show

instance Default GetStreamsOptions where
  def = GetStreamsOptions
    { getStreamsOptions_resolution = Nothing
    , getStreamsOptions_seriesType = "distance"
    }

instance QueryLike GetStreamsOptions where
  toQuery options = toQuery
    [ ("resolution", getStreamsOptions_resolution options)
    , ("distance", Just (getStreamsOptions_seriesType options))
    ]

-- * Uploads

-- | 'Strive.Actions.uploadActivity'
data UploadActivityOptions = UploadActivityOptions
  { uploadActivityOptions_activityType :: Maybe ActivityType
  , uploadActivityOptions_name         :: Maybe String
  , uploadActivityOptions_description  :: Maybe String
  , uploadActivityOptions_private      :: Bool
  , uploadActivityOptions_trainer      :: Bool
  , uploadActivityOptions_externalId   :: Maybe String
  } deriving Show

instance Default UploadActivityOptions where
  def = UploadActivityOptions
    { uploadActivityOptions_activityType = Nothing
    , uploadActivityOptions_name = Nothing
    , uploadActivityOptions_description = Nothing
    , uploadActivityOptions_private = False
    , uploadActivityOptions_trainer = False
    , uploadActivityOptions_externalId = Nothing
    }

instance QueryLike UploadActivityOptions where
  toQuery options = toQuery
    [ ("activity_type", fmap show (uploadActivityOptions_activityType options))
    , ("name", uploadActivityOptions_name options)
    , ("description", uploadActivityOptions_description options)
    , ("private", Just (show (fromEnum (uploadActivityOptions_private options))))
    , ("trainer", Just (show (fromEnum (uploadActivityOptions_trainer options))))
    , ("external_id", uploadActivityOptions_externalId options)
    ]
