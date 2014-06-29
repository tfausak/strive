{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/streams/>
module Strive.Actions.Streams
    ( getActivityStreams
    , getEffortStreams
    , getSegmentStreams
    ) where

import Data.Aeson (FromJSON, Value)
import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Strive.Client (Client)
import Strive.Client.HTTP (get)
import Strive.Objects (StreamDetailed)
import Strive.Types (ActivityId, EffortId, Resolution, SegmentId, SeriesType,
                     StreamTypes)
import Strive.Utilities (queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/streams/#activity>
getActivityStreams :: Client -> ActivityId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getActivityStreams = flip getStreams "activities"

-- | <http://strava.github.io/api/v3/streams/#effort>
getEffortStreams :: Client -> EffortId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getEffortStreams = flip getStreams "segment_efforts"

-- | <http://strava.github.io/api/v3/streams/#segment>
getSegmentStreams :: Client -> SegmentId -> StreamTypes -> Resolution -> SeriesType -> IO (Either String [StreamDetailed])
getSegmentStreams = flip getStreams "segments"

getStreams :: FromJSON a => Client -> String -> Integer -> StreamTypes -> Resolution -> SeriesType -> IO (Either String a)
getStreams client resource id streamTypes resolution seriesType =
    get client resource' query
  where
    resource' = concat
        [ resource
        , "/"
        , show id
        , "/streams/"
        , intercalate "," streamTypes
        ]
    query = queryToSimpleQuery
        [ ("resolution", fmap pack resolution)
        , ("series_type", fmap pack seriesType)
        ]
