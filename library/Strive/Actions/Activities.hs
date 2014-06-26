{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/>
module Strive.Actions.Activities
    ( getActivity
    , getActivityLaps
    , getActivityZones
    , getCurrentActivities
    , getFeed
    ) where

import           Data.Aeson            (encode)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Strive.Client         (Client)
import           Strive.Objects        (ActivitySummary, EffortLap,
                                        ZoneSummary)
import           Strive.Types          (ActivityId, Page, PerPage)
import           Strive.Utilities      (get, paginate, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity :: Client -> ActivityId -> Maybe Bool -> IO (Either String ActivitySummary)
getActivity client activityId allEfforts = get client resource query
  where
    resource = "activities/" <> show activityId
    query = queryToSimpleQuery
        [ ("include_all_efforts", fmap (toStrict . encode) allEfforts)
        ]

-- | <http://strava.github.io/api/v3/activities/#laps>
getActivityLaps :: Client -> ActivityId -> IO (Either String [EffortLap])
getActivityLaps client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/laps"
    query = []

-- | <http://strava.github.io/api/v3/activities/#zones>
getActivityZones :: Client -> ActivityId -> IO (Either String [ZoneSummary])
getActivityZones client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/zones"
    query = []

-- | <http://strava.github.io/api/v3/activities/#get-activities>
getCurrentActivities :: Client -> Maybe UTCTime -> Maybe UTCTime -> Page -> PerPage -> IO (Either String [ActivitySummary])
getCurrentActivities client before after page perPage = get client resource query
  where
    resource = "athlete/activities"
    query = paginate page perPage <> queryToSimpleQuery
        [ ("before", fmap (pack . show . utcTimeToPOSIXSeconds) before)
        , ("after", fmap (pack . show . utcTimeToPOSIXSeconds) after)
        ]

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> Page -> PerPage -> IO (Either String [ActivitySummary])
getFeed client page perPage = get client resource query
  where
    resource = "activities/following"
    query = paginate page perPage
