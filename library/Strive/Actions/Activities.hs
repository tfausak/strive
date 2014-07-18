-- | <http://strava.github.io/api/v3/activities/>
module Strive.Actions.Activities
  ( createActivity
  , getActivity
  , updateActivity
  , deleteActivity
  , getCurrentActivities
  , getFeed
  , getActivityZones
  , getActivityLaps
  ) where

-- | <http://strava.github.io/api/v3/activities/#create>
createActivity :: Client -> String -> String -> UTCTime -> Integer -> O.CreateActivityOptions -> IO (Either String T.ActivityDetailed)
createActivity client name type_ startDateLocal elapsedTime options = post client resource query
 where
  resource = "api/v3/activities"
  query = toQuery
    [ ("name", name)
    , ("type", type_)
    , ("start_date_local", unpack (toStrict (encode startDateLocal)))
    , ("elapsed_time", show elapsedTime)
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity :: Client -> Integer -> O.GetActivityOptions -> IO (Either String T.ActivitySummary)
getActivity client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#put-updates>
updateActivity :: Client -> Integer -> O.UpdateActivityOptions -> IO (Either String T.ActivityDetailed)
updateActivity client activityId options = put client resource query
 where
  resource = "api/v3/activities/" <> show activityId
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
  resource = "api/v3/activities/" <> show activityId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#get-activities>
getCurrentActivities :: Client -> O.GetCurrentActivitiesOptions -> IO (Either String [T.ActivitySummary])
getCurrentActivities client options = get client resource query
 where
  resource = "api/v3/athlete/activities"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> O.GetFeedOptions -> IO (Either String [T.ActivitySummary])
getFeed client options = get client resource query
 where
  resource = "api/v3/activities/following"
  query = toQuery options

-- | <http://strava.github.io/api/v3/activities/#zones>
getActivityZones :: Client -> Integer -> IO (Either String [T.ActivityZoneDetailed])
getActivityZones client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/zones"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/activities/#laps>
getActivityLaps :: Client -> Integer -> IO (Either String [T.ActivityLapSummary])
getActivityLaps client activityId = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/laps"
  query = [] :: Query
