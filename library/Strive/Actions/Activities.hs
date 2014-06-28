{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/>
module Strive.Actions.Activities
    ( getActivity
    , getActivityLaps
    , getActivityZones
    , getCurrentActivities
    , getFeed
    , putActivity
    ) where

import           Data.Aeson            (encode)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Data.Time.Clock       (UTCTime)
import           Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import           Strive.Client         (Client)
import           Strive.Objects        (ActivityDetailed, ActivitySummary,
                                        EffortLap, ZoneSummary)
import           Strive.Types          (ActivityId, Page, PerPage)
import           Strive.Utilities      (get, paginate, put, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/activities/#get-details>
getActivity :: Client -> ActivityId -> Maybe Bool -> IO (Either String ActivitySummary)
getActivity client activityId allEfforts = get client resource query
  where
    resource = "activities/" <> show activityId
    query = queryToSimpleQuery
        [ (pack "include_all_efforts", fmap (toStrict . encode) allEfforts)
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
        [ (pack "before", fmap (pack . show . utcTimeToPOSIXSeconds) before)
        , (pack "after", fmap (pack . show . utcTimeToPOSIXSeconds) after)
        ]

-- | <http://strava.github.io/api/v3/activities/#get-feed>
getFeed :: Client -> Page -> PerPage -> IO (Either String [ActivitySummary])
getFeed client page perPage = get client resource query
  where
    resource = "activities/following"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/activities/#put-updates>
putActivity :: Client -> ActivityId -> Maybe String -> Maybe String -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe String -> Maybe String -> IO (Either String ActivityDetailed)
putActivity client activityId name type_ private commute trainer gearId description = put client resource query
  where
    resource = "activities/" <> show activityId
    query = queryToSimpleQuery
        [ ("name", fmap pack name)
        , ("type", fmap pack type_)
        , ("private", fmap (toStrict . encode) private)
        , ("commute", fmap (toStrict . encode) commute)
        , ("trainer", fmap (toStrict . encode) trainer)
        , ("gear_id", fmap pack gearId)
        , ("description", fmap pack description)
        ]
