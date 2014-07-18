module Strive.Actions.Friends
  ( getCurrentFriends
  , getFriends
  , getCurrentFollowers
  , getFollowers
  , getCommonFriends
  ) where

import Data.Monoid ((<>))
import Network.HTTP.Types (toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (GetCommonFriendsOptions, GetCurrentFollowersOptions,
                       GetCurrentFriendsOptions, GetFollowersOptions,
                       GetFriendsOptions)
import Strive.Types (AthleteSummary)

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> GetCurrentFriendsOptions -> IO (Either String [AthleteSummary])
getCurrentFriends client options = get client resource query
 where
  resource = "api/v3/athlete/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Integer -> GetFriendsOptions -> IO (Either String [AthleteSummary])
getFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFollowers :: Client -> GetCurrentFollowersOptions -> IO (Either String [AthleteSummary])
getCurrentFollowers client options = get client resource query
 where
  resource = "api/v3/athlete/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> Integer -> GetFollowersOptions -> IO (Either String [AthleteSummary])
getFollowers client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> Integer -> GetCommonFriendsOptions -> IO (Either String [AthleteSummary])
getCommonFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/both-following"
  query = toQuery options
