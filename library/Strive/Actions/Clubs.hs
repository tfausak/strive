-- | <http://strava.github.io/api/v3/clubs/>
module Strive.Actions.Clubs
  ( getClub
  , getCurrentClubs
  , getClubMembers
  , getClubActivities
  , joinClub
  , leaveClub
  ) where

import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Conduit (responseBody, responseStatus)
import Network.HTTP.Types (Query, methodPost, ok200, toQuery)
import Strive.Aliases (ClubId, Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (buildRequest, get, performRequest)
import Strive.Options (GetClubActivitiesOptions, GetClubMembersOptions)
import Strive.Types (ActivitySummary, AthleteSummary, ClubDetailed,
                     ClubSummary)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> ClubId -> Result ClubDetailed
getClub client clubId = get client resource query
 where
  resource = "api/v3/clubs/" ++ show clubId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> Result [ClubSummary]
getCurrentClubs client = get client resource query
  where
    resource = "api/v3/athlete/clubs"
    query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> ClubId -> GetClubMembersOptions -> Result [AthleteSummary]
getClubMembers client clubId options = get client resource query
  where
    resource = "api/v3/clubs/" ++ show clubId ++ "/members"
    query = toQuery options

-- | <http://strava.github.io/api/v3/clubs/#get-activities>
getClubActivities :: Client -> ClubId -> GetClubActivitiesOptions -> Result [ActivitySummary]
getClubActivities client clubId options = get client resource query
 where
  resource = "api/v3/clubs/" ++ show clubId ++ "/activities"
  query = toQuery options

-- | <http://strava.github.io/api/v3/clubs/#join>
joinClub :: Client -> ClubId -> Result ()
joinClub client clubId = do
  request <- buildRequest methodPost client resource query
  response <- performRequest client request
  return (if responseStatus response == ok200
    then Right ()
    else Left ((unpack . toStrict . responseBody) response))
 where
  resource = "api/v3/clubs/" ++ show clubId ++ "/join"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/clubs/#leave>
leaveClub :: Client -> ClubId -> Result ()
leaveClub client clubId = do
  request <- buildRequest methodPost client resource query
  response <- performRequest client request
  return (if responseStatus response == ok200
    then Right ()
    else Left ((unpack . toStrict . responseBody) response))
 where
  resource = "api/v3/clubs/" ++ show clubId ++ "/leave"
  query = [] :: Query
