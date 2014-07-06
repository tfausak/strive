-- | Optional parameters for actions.
module Strive.Options where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)

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
  , updateCurrentAthleteOptions_sex     :: Maybe Char
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
    , ("sex", fmap (: []) (updateCurrentAthleteOptions_sex options))
    , ("weight", fmap show (updateCurrentAthleteOptions_weight options))
    ]

-- | 'Strive.Actions.getAthleteCrs'
data GetAthleteCrsOptions = GetAthleteCrsOptions
  { getAthleteCrsOptions_page    :: Integer
  , getAthleteCrsOptions_perPage :: Integer
  } deriving Show

instance Default GetAthleteCrsOptions where
  def = GetAthleteCrsOptions
    { getAthleteCrsOptions_page = 1
    , getAthleteCrsOptions_perPage = 200
    }

instance QueryLike GetAthleteCrsOptions where
  toQuery options = toQuery
    [ ("page", show (getAthleteCrsOptions_page options))
    , ("per_page", show (getAthleteCrsOptions_perPage options))
    ]

-- * Friends and Followers

-- | 'Strive.Actions.getCurrentFriends'
data GetCurrentFriendsOptions = GetCurrentFriendsOptions
  { getCurrentFriendsOptions_page    :: Integer
  , getCurrentFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetCurrentFriendsOptions where
  def = GetCurrentFriendsOptions
    { getCurrentFriendsOptions_page = 1
    , getCurrentFriendsOptions_perPage = 200
    }

instance QueryLike GetCurrentFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getCurrentFriendsOptions_page options))
    , ("per_page", show (getCurrentFriendsOptions_perPage options))
    ]

-- | 'Strive.Actions.getFriends'
data GetFriendsOptions = GetFriendsOptions
  { getFriendsOptions_page    :: Integer
  , getFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetFriendsOptions where
  def = GetFriendsOptions
    { getFriendsOptions_page = 1
    , getFriendsOptions_perPage = 200
    }

instance QueryLike GetFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getFriendsOptions_page options))
    , ("per_page", show (getFriendsOptions_perPage options))
    ]

-- | 'Strive.Actions.getCurrentFollowers'
data GetCurrentFollowersOptions = GetCurrentFollowersOptions
  { getCurrentFollowersOptions_page    :: Integer
  , getCurrentFollowersOptions_perPage :: Integer
  } deriving Show

instance Default GetCurrentFollowersOptions where
  def = GetCurrentFollowersOptions
    { getCurrentFollowersOptions_page = 1
    , getCurrentFollowersOptions_perPage = 200
    }

instance QueryLike GetCurrentFollowersOptions where
  toQuery options = toQuery
    [ ("page", show (getCurrentFollowersOptions_page options))
    , ("per_page", show (getCurrentFollowersOptions_perPage options))
    ]

-- | 'Strive.Actions.getFollowers'
data GetFollowersOptions = GetFollowersOptions
  { getFollowersOptions_page    :: Integer
  , getFollowersOptions_perPage :: Integer
  } deriving Show

instance Default GetFollowersOptions where
  def = GetFollowersOptions
    { getFollowersOptions_page = 1
    , getFollowersOptions_perPage = 200
    }

instance QueryLike GetFollowersOptions where
  toQuery options = toQuery
    [ ("page", show (getFollowersOptions_page options))
    , ("per_page", show (getFollowersOptions_perPage options))
    ]

-- | 'Strive.Actions.getCommonFriends'
data GetCommonFriendsOptions = GetCommonFriendsOptions
  { getCommonFriendsOptions_page    :: Integer
  , getCommonFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetCommonFriendsOptions where
  def = GetCommonFriendsOptions
    { getCommonFriendsOptions_page = 1
    , getCommonFriendsOptions_perPage = 200
    }

instance QueryLike GetCommonFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getCommonFriendsOptions_page options))
    , ("per_page", show (getCommonFriendsOptions_perPage options))
    ]

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
  , updateActivityOptions_type        :: Maybe String
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
    , ("type", updateActivityOptions_type options)
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
data GetFeedOptions = GetFeedOptions
  { getFeedOptions_page    :: Integer
  , getFeedOptions_perPage :: Integer
  } deriving Show

instance Default GetFeedOptions where
  def = GetFeedOptions
    { getFeedOptions_page = 1
    , getFeedOptions_perPage = 200
    }

instance QueryLike GetFeedOptions where
  toQuery options = toQuery
    [ ("page", Just (show (getFeedOptions_page options)))
    , ("per_page", Just (show (getFeedOptions_perPage options)))
    ]

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
data GetActivityKudoersOptions = GetActivityKudoersOptions
  { getActivityKudoersOptions_page    :: Integer
  , getActivityKudoersOptions_perPage :: Integer
  } deriving Show

instance Default GetActivityKudoersOptions where
  def = GetActivityKudoersOptions
    { getActivityKudoersOptions_page = 1
    , getActivityKudoersOptions_perPage = 200
    }

instance QueryLike GetActivityKudoersOptions where
  toQuery options = toQuery
    [ ("page", Just (show (getActivityKudoersOptions_page options)))
    , ("per_page", Just (show (getActivityKudoersOptions_perPage options)))
    ]

-- * Clubs

-- | 'Strive.Actions.getClubMembers'
data GetClubMembersOptions = GetClubMembersOptions
  { getClubMembersOptions_page    :: Integer
  , getClubMembersOptions_perPage :: Integer
  } deriving Show

instance Default GetClubMembersOptions where
  def = GetClubMembersOptions
    { getClubMembersOptions_page = 1
    , getClubMembersOptions_perPage = 200
    }

instance QueryLike GetClubMembersOptions where
  toQuery options = toQuery
    [ ("page", Just (show (getClubMembersOptions_page options)))
    , ("per_page", Just (show (getClubMembersOptions_perPage options)))
    ]

-- | 'Strive.Actions.getClubActivities'
data GetClubActivitiesOptions = GetClubActivitiesOptions
  { getClubActivitiesOptions_page    :: Integer
  , getClubActivitiesOptions_perPage :: Integer
  } deriving Show

instance Default GetClubActivitiesOptions where
  def = GetClubActivitiesOptions
    { getClubActivitiesOptions_page = 1
    , getClubActivitiesOptions_perPage = 200
    }

instance QueryLike GetClubActivitiesOptions where
  toQuery options = toQuery
    [ ("page", Just (show (getClubActivitiesOptions_page options)))
    , ("per_page", Just (show (getClubActivitiesOptions_perPage options)))
    ]

-- | 'Strive.Actions.getStarredSegments'
data GetStarredSegmentsOptions = GetStarredSegmentsOptions
  { getStarredSegmentsOptions_page    :: Integer
  , getStarredSegmentsOptions_perPage :: Integer
  } deriving Show

instance Default GetStarredSegmentsOptions where
  def = GetStarredSegmentsOptions
    { getStarredSegmentsOptions_page = 1
    , getStarredSegmentsOptions_perPage = 200
    }

instance QueryLike GetStarredSegmentsOptions where
  toQuery options = toQuery
    [ ("page", Just (show (getStarredSegmentsOptions_page options)))
    , ("per_page", Just (show (getStarredSegmentsOptions_perPage options)))
    ]

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
  { getSegmentLeaderboard_gender      :: Maybe Char
  , getSegmentLeaderboard_ageGroup    :: Maybe String
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
    [ ("gender", fmap (: []) (getSegmentLeaderboard_gender options))
    , ("age_group", getSegmentLeaderboard_ageGroup options)
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
