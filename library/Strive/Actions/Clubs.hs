-- | <http://strava.github.io/api/v3/clubs/>
module Strive.Actions.Clubs
  ( getClub
  , getCurrentClubs
  , getClubMembers
  , getClubActivities
  ) where

import Network.HTTP.Types (Query, toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (GetClubActivitiesOptions, GetClubMembersOptions)
import Strive.Types (ActivitySummary, AthleteSummary, ClubDetailed,
                     ClubSummary)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Either String ClubDetailed)
getClub client clubId = get client resource query
 where
  resource = "api/v3/clubs/" ++ show clubId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "api/v3/athlete/clubs"
    query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> Integer -> GetClubMembersOptions -> IO (Either String [AthleteSummary])
getClubMembers client clubId options = get client resource query
  where
    resource = "api/v3/clubs/" ++ show clubId ++ "/members"
    query = toQuery options

-- | <http://strava.github.io/api/v3/clubs/#get-activities>
getClubActivities :: Client -> Integer -> GetClubActivitiesOptions -> IO (Either String [ActivitySummary])
getClubActivities client clubId options = get client resource query
 where
  resource = "api/v3/clubs/" ++ show clubId ++ "/activities"
  query = toQuery options
