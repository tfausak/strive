{-# LANGUAGE OverloadedStrings #-}

-- | Functions for performing actions against the API.
module Scurry.Actions
    ( getAthlete
    , getClub
    , getClubMembers
    , getComments
    , getCommonFriends
    , getCurrentClubs
    , getCurrentFollowers
    , getCurrentFriends
    , getFollowers
    , getFriends
    , getGear
    , getKudoers
    , getPhotos
    , getZones
    ) where

import           Data.Aeson              (encode)
import           Data.ByteString.Lazy    (toStrict)
import           Data.Monoid             ((<>))
import           Scurry.Actions.Internal (get, paginate)
import           Scurry.Client           (Client)
import           Scurry.Objects          (AthleteSummary, ClubDetailed,
                                          ClubSummary, CommentSummary,
                                          GearDetailed, PhotoSummary,
                                          ZoneSummary)

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Integer -> IO (Either String AthleteSummary)
getAthlete client athleteId = get client resource query
  where
    resource = "athletes/" <> show athleteId
    query = []

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Either String ClubDetailed)
getClub client clubId = get client resource query
  where
    resource = "clubs/" <> show clubId
    query = []

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getClubMembers client clubId page perPage = get client resource query
  where
    resource = "clubs/" <> show clubId <> "/members"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/comments/#list>
getComments :: Client -> Integer -> Bool -> Integer -> Integer -> IO (Either String [CommentSummary])
getComments client activityId includeMarkdown page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/comments"
    query = ("markdown", toStrict (encode includeMarkdown)) : paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getCommonFriends client athleteId page perPage = get client resource query
  where
    resource = "/athletes/" <> show athleteId <> "/both-following"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "athlete/clubs"
    query = []

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFollowers :: Client -> Integer -> Integer -> IO (Either String [AthleteSummary])
getCurrentFollowers client page perPage = get client resource query
  where
    resource = "athlete/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> Integer -> Integer -> IO (Either String [AthleteSummary])
getCurrentFriends client page perPage = get client resource query
  where
    resource = "athlete/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getFollowers client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> String -> IO (Either String GearDetailed)
getGear client gearId = get client resource query
  where
    resource = "gear/" <> gearId
    query = []

-- | <http://strava.github.io/api/v3/kudos/#list>
getKudoers :: Client -> Integer -> Integer -> Integer -> IO (Either String [AthleteSummary])
getKudoers client activityId page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/kudos"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/photos/#list>
getPhotos :: Client -> Integer -> IO (Either String [PhotoSummary])
getPhotos client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/photos"
    query = []

-- | <http://strava.github.io/api/v3/activities/#zones>
getZones :: Client -> Integer -> IO (Either String [ZoneSummary])
getZones client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/zones"
    query = []
