-- | Functions for performing actions against the API.
module Scurry.Actions
    ( getClub
    , getCurrentFriends
    , getFriends
    ) where

import           Data.Monoid             ((<>))
import           Scurry.Actions.Internal (get, paginate)
import           Scurry.Client           (Client)
import           Scurry.Objects          (AthleteSummary, ClubDetailed)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Either String ClubDetailed)
getClub client clubId = get client resource query
  where
    resource = "clubs/" <> show clubId
    query = []

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> Integer -> Integer -> IO (Either String [AthleteSummary])
getCurrentFriends client page perPage = get client resource query
  where
    resource = "athlete/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/friends"
    query = paginate page perPage
