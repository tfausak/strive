{-# LANGUAGE OverloadedStrings #-}

-- | Functions for performing actions against the API.
module Strive.Actions
    ( getComments
    , getCommonFriends
    , getCurrentFollowers
    , getCurrentFriends
    , getFollowers
    , getFriends
    , getKudoers
    , getPhotos
    , module Actions
    ) where

import           Data.Aeson                (encode)
import           Data.ByteString.Lazy      (toStrict)
import           Data.Monoid               ((<>))
import           Strive.Actions.Activities as Actions
import           Strive.Actions.Athletes   as Actions
import           Strive.Actions.Clubs      as Actions
import           Strive.Actions.Efforts    as Actions
import           Strive.Actions.Gear       as Actions
import           Strive.Actions.Internal   (get, paginate)
import           Strive.Actions.Segments   as Actions
import           Strive.Client             (Client)
import qualified Strive.Objects            as Objects
import qualified Strive.Types              as Types

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
