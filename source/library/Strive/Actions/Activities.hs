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
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types (Query, methodDelete, noContent204, toQuery)
import Strive.Aliases (ActivityId, ElapsedTime, Name, Result, StartTime)
import Strive.Client (Client)
import Strive.Enums (ActivityType)
import Strive.Internal.HTTP (buildRequest, get, performRequest, post, put)
import Strive.Options
  ( CreateActivityOptions
  , GetActivityOptions
  , GetCurrentActivitiesOptions
  , GetFeedOptions
  , GetRelatedActivitiesOptions
  , UpdateActivityOptions
  )
import Strive.Types
  (ActivityDetailed, ActivityLapSummary, ActivitySummary, ActivityZoneDetailed)

-- | <http://strava.github.io/api/v3/activities/#create>
createActivity
  :: Client
  -> Name
  -> ActivityType
  -> StartTime
  -> ElapsedTime
  -> CreateActivityOptions
  -> IO (Result ActivityDetailed)
createActivity client name type_ startDateLocal elapsedTime options = post
  client
  resource
  query
 where
  resource = "api/v3/activities"
  query =
    toQuery
        [ ("name", name)
        , ("type", show type_)
        , ("start_date_local", unpack (toStrict (encode startDateLocal)))
        , ("elapsed_time", show elapsedTime)
        ]
      <> toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity
  :: Client -> ActivityId -> GetActivityOptions -> IO (Result ActivitySummary)
getActivity client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#put-updates>
updateActivity
  :: Client
  -> ActivityId
  -> UpdateActivityOptions
  -> IO (Result ActivityDetailed)
updateActivity client activityId options = put client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#delete>
deleteActivity :: Client -> ActivityId -> IO (Result ())
deleteActivity client activityId = do
  request <- buildRequest methodDelete client resource query
  response <- performRequest client request
  return
    (if responseStatus response == noContent204
      then Right ()
      else Left (response, unpack (toStrict (responseBody response)))
    )
 where
  resource = "api/v3/activities/" <> show activityId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#get-activities>
getCurrentActivities
  :: Client -> GetCurrentActivitiesOptions -> IO (Result [ActivitySummary])
getCurrentActivities client options = get client resource query
 where
  resource = "api/v3/athlete/activities"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-related>
getRelatedActivities
  :: Client
  -> ActivityId
  -> GetRelatedActivitiesOptions
  -> IO (Result [ActivitySummary])
getRelatedActivities client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/related"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> GetFeedOptions -> IO (Result [ActivitySummary])
getFeed client options = get client resource query
 where
  resource = "api/v3/activities/following"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#zones>
getActivityZones :: Client -> ActivityId -> IO (Result [ActivityZoneDetailed])
getActivityZones client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/zones"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#laps>
getActivityLaps :: Client -> ActivityId -> IO (Result [ActivityLapSummary])
getActivityLaps client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/laps"
  query = [] :: Query
