-- | <http://strava.github.io/api/v3/follow/>
module Strive.Actions.Friends
    ( getCommonFriends
    , getCurrentFollowers
    , getCurrentFriends
    , getFollowers
    , getFriends
    ) where

import           Data.Monoid             ((<>))
import           Strive.Actions.Internal (get, paginate)
import           Strive.Client           (Client)
import           Strive.Objects          (AthleteSummary)
import           Strive.Types            (AthleteId, Page, PerPage)

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> AthleteId -> Page -> PerPage -> IO (Either String [AthleteSummary])
getCommonFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/both-following"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFollowers :: Client -> Page -> PerPage -> IO (Either String [AthleteSummary])
getCurrentFollowers client page perPage = get client resource query
  where
    resource = "athlete/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFriends :: Client -> Page -> PerPage -> IO (Either String [AthleteSummary])
getCurrentFriends client page perPage = get client resource query
  where
    resource = "athlete/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> AthleteId -> Page -> PerPage -> IO (Either String [AthleteSummary])
getFollowers client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> AthleteId -> Page -> PerPage -> IO (Either String [AthleteSummary])
getFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/friends"
    query = paginate page perPage
