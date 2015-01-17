-- | <http://strava.github.io/api/v3/activities/>
module Strive.Actions.Activities
  ( createActivity
  , getActivity
  , updateActivity
  , deleteActivity
  , getCurrentActivities
  , getRelatedActivities
  , getFeed
  , getActivityZones
  , getActivityLaps
  ) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (responseBody, responseStatus)
import Network.HTTP.Types (Query, methodDelete, noContent204, toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (buildRequest, get, performRequest, post, put)
import Strive.Options (CreateActivityOptions, GetActivityOptions,
                       GetCurrentActivitiesOptions, GetFeedOptions,
                       GetRelatedActivitiesOptions, UpdateActivityOptions)
import Strive.Types (ActivityDetailed, ActivityLapSummary, ActivitySummary,
                     ActivityZoneDetailed)

-- | <http://strava.github.io/api/v3/activities/#create>
createActivity :: Client -> String -> String -> UTCTime -> Integer -> CreateActivityOptions -> IO (Either String ActivityDetailed)
createActivity client name type_ startDateLocal elapsedTime options = post client resource query
 where
  resource = "api/v3/activities"
  query = toQuery
    [ ("name", name)
    , ("type", type_)
    , ("start_date_local", unpack (toStrict (encode startDateLocal)))
    , ("elapsed_time", show elapsedTime)
    ] ++ toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity :: Client -> Integer -> GetActivityOptions -> IO (Either String ActivitySummary)
getActivity client activityId options = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#put-updates>
updateActivity :: Client -> Integer -> UpdateActivityOptions -> IO (Either String ActivityDetailed)
updateActivity client activityId options = put client resource query
 where
  resource = "api/v3/activities/" ++ show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#delete>
deleteActivity :: Client -> Integer -> IO (Either String ())
deleteActivity client activityId = do
  request <- buildRequest methodDelete client resource query
  response <- performRequest client request
  return (if responseStatus response == noContent204
    then Right ()
    else Left (unpack (toStrict (responseBody response))))
 where
  resource = "api/v3/activities/" ++ show activityId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#get-activities>
getCurrentActivities :: Client -> GetCurrentActivitiesOptions -> IO (Either String [ActivitySummary])
getCurrentActivities client options = get client resource query
 where
  resource = "api/v3/athlete/activities"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-related>
getRelatedActivities :: Client -> Integer -> GetRelatedActivitiesOptions -> IO (Either String [ActivitySummary])
getRelatedActivities client activityId options = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/related"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> GetFeedOptions -> IO (Either String [ActivitySummary])
getFeed client options = get client resource query
 where
  resource = "api/v3/activities/following"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#zones>
getActivityZones :: Client -> Integer -> IO (Either String [ActivityZoneDetailed])
getActivityZones client activityId = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/zones"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#laps>
getActivityLaps :: Client -> Integer -> IO (Either String [ActivityLapSummary])
getActivityLaps client activityId = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/laps"
  query = [] :: Query
