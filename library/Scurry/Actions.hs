{-# LANGUAGE OverloadedStrings #-}

-- | Functions for performing actions against the API.
module Scurry.Actions
    ( getAthlete
    , getAthleteCRs
    , getClub
    , getClubMembers
    , getComments
    , getCommonFriends
    , getCurrentAthlete
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
import qualified Scurry.Objects          as Objects
import qualified Scurry.Types            as Types

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Types.AthleteId -> IO (Either String Objects.AthleteSummary)
getAthlete client athleteId = get client resource query
  where
    resource = "athletes/" <> show athleteId
    query = []

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCRs :: Client -> Types.AthleteId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.EffortSummary])
getAthleteCRs client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/koms"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Types.ClubId -> IO (Either String Objects.ClubDetailed)
getClub client clubId = get client resource query
  where
    resource = "clubs/" <> show clubId
    query = []

-- | <http://strava.github.io/api/v3/clubs/#get-members>
getClubMembers :: Client -> Types.ClubId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getClubMembers client clubId page perPage = get client resource query
  where
    resource = "clubs/" <> show clubId <> "/members"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/comments/#list>
getComments :: Client -> Types.ActivityId -> Types.IncludeMarkdown -> Types.Page -> Types.PerPage -> IO (Either String [Objects.CommentSummary])
getComments client activityId includeMarkdown page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/comments"
    query = ("markdown", toStrict (encode includeMarkdown)) : paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#both>
getCommonFriends :: Client -> Types.AthleteId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getCommonFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/both-following"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String Objects.AthleteDetailed)
getCurrentAthlete client = get client resource query
  where
    resource = "athlete"
    query = []

-- | <http://strava.github.io/api/v3/clubs/#get-athletes>
getCurrentClubs :: Client -> IO (Either String [Objects.ClubSummary])
getCurrentClubs client = get client resource query
  where
    resource = "athlete/clubs"
    query = []

-- | <http://strava.github.io/api/v3/follow/#followers>
getCurrentFollowers :: Client -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getCurrentFollowers client page perPage = get client resource query
  where
    resource = "athlete/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getCurrentFriends :: Client -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getCurrentFriends client page perPage = get client resource query
  where
    resource = "athlete/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#followers>
getFollowers :: Client -> Types.AthleteId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getFollowers client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/followers"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/follow/#friends>
getFriends :: Client -> Types.AthleteId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getFriends client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/friends"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> Types.GearId -> IO (Either String Objects.GearDetailed)
getGear client gearId = get client resource query
  where
    resource = "gear/" <> gearId
    query = []

-- | <http://strava.github.io/api/v3/kudos/#list>
getKudoers :: Client -> Types.ActivityId -> Types.Page -> Types.PerPage -> IO (Either String [Objects.AthleteSummary])
getKudoers client activityId page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/kudos"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/photos/#list>
getPhotos :: Client -> Types.ActivityId -> IO (Either String [Objects.PhotoSummary])
getPhotos client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/photos"
    query = []

-- | <http://strava.github.io/api/v3/activities/#zones>
getZones :: Client -> Types.ActivityId -> IO (Either String [Objects.ZoneSummary])
getZones client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/zones"
    query = []
