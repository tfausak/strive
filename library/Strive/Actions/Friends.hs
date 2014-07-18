module Strive.Actions.Friends
  ( getCurrentFriends
  , getFriends
  , getCurrentFollowers
  , getFollowers
  , getCommonFriends
  ) where

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> O.GetCurrentFriendsOptions -> IO (Either String [T.AthleteSummary])
getCurrentFriends client options = get client resource query
 where
  resource = "api/v3/athlete/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Integer -> O.GetFriendsOptions -> IO (Either String [T.AthleteSummary])
getFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/friends"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFollowers :: Client -> O.GetCurrentFollowersOptions -> IO (Either String [T.AthleteSummary])
getCurrentFollowers client options = get client resource query
 where
  resource = "api/v3/athlete/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> Integer -> O.GetFollowersOptions -> IO (Either String [T.AthleteSummary])
getFollowers client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/followers"
  query = toQuery options

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> Integer -> O.GetCommonFriendsOptions -> IO (Either String [T.AthleteSummary])
getCommonFriends client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/both-following"
  query = toQuery options
