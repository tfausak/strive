module Strive.Actions.Clubs
  ( getClub
  , getCurrentClubs
  , getClubMembers
  , getClubActivities
  ) where

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Either String T.ClubDetailed)
getClub client clubId = get client resource query
 where
  resource = "api/v3/clubs/" <> show clubId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [T.ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "api/v3/athlete/clubs"
    query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> Integer -> O.GetClubMembersOptions -> IO (Either String [T.AthleteSummary])
getClubMembers client clubId options = get client resource query
  where
    resource = "api/v3/clubs/" <> show clubId <> "/members"
    query = toQuery options

-- | <http://strava.github.io/api/v3/clubs/#get-activities>
getClubActivities :: Client -> Integer -> O.GetClubActivitiesOptions -> IO (Either String [T.ActivitySummary])
getClubActivities client clubId options = get client resource query
 where
  resource = "api/v3/clubs/" <> show clubId <> "/activities"
  query = toQuery options
