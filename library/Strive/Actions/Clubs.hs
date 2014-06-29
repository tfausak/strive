-- | <http://strava.github.io/api/v3/clubs/>
module Strive.Actions.Clubs
    ( getClub
    , getClubActivities
    , getClubMembers
    , getCurrentClubs
    ) where

import Data.Monoid ((<>))
import Strive.Client (Client)
import Strive.Client.HTTP (get)
import Strive.Objects (ActivitySummary, AthleteSummary, ClubDetailed,
                       ClubSummary)
import Strive.Types (ClubId, Page, PerPage)
import Strive.Utilities (paginate)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> ClubId -> IO (Either String ClubDetailed)
getClub client clubId = get client resource query
  where
    resource = "clubs/" <> show clubId
    query = [] :: [(String, String)]

-- | <http://strava.github.io/api/v3/clubs/#get-activities>
getClubActivities :: Client -> ClubId -> Page -> PerPage -> IO (Either String [ActivitySummary])
getClubActivities client clubId page perPage = get client resource query
  where
    resource = "clubs/" <> show clubId <> "/activities"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> ClubId -> Page -> PerPage -> IO (Either String [AthleteSummary])
getClubMembers client clubId page perPage = get client resource query
  where
    resource = "clubs/" <> show clubId <> "/members"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "athlete/clubs"
    query = [] :: [(String, String)]
