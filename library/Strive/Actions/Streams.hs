{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/streams/>
module Strive.Actions.Streams
    ( getActivityStreams
    , getEffortStreams
    , getSegmentStreams
    ) where

import           Data.Aeson            (Value)
import           Data.ByteString.Char8 (pack)
import           Data.List             (intercalate)
import           Strive.Client         (Client)
import           Strive.Objects        (StreamDetailed)
import           Strive.Types          (ActivityId, EffortId, Resolution,
                                        SegmentId, SeriesType, StreamTypes)
import           Strive.Utilities      (get, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/streams/#activity>
getActivityStreams :: Client -> ActivityId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getActivityStreams client activityId streamTypes resolution seriesType = get client resource query
  where
    resource = concat
        [ "activities/"
        , show activityId
        , "/streams/"
        , intercalate "," streamTypes
        ]
    query = queryToSimpleQuery
        [ ("resolution", fmap pack resolution)
        , ("series_type", fmap pack seriesType)
        ]

-- | <http://strava.github.io/api/v3/streams/#effort>
getEffortStreams :: Client -> EffortId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getEffortStreams client effortId streamTypes resolution seriesType = get client resource query
  where
    resource = concat
        [ "segment_efforts/"
        , show effortId
        , "/streams/"
        , intercalate "," streamTypes
        ]
    query = queryToSimpleQuery
        [ ("resolution", fmap pack resolution)
        , ("series_type", fmap pack seriesType)
        ]

-- | <http://strava.github.io/api/v3/streams/#segment>
getSegmentStreams :: Client -> SegmentId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getSegmentStreams client segmentId streamTypes resolution seriesType = get client resource query
  where
    resource = concat
        [ "segments/"
        , show segmentId
        , "/streams/"
        , intercalate "," streamTypes
        ]
    query = queryToSimpleQuery
        [ ("resolution", fmap pack resolution)
        , ("series_type", fmap pack seriesType)
        ]
