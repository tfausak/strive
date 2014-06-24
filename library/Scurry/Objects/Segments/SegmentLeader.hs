{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
module Scurry.Objects.Segments.SegmentLeader
    ( SegmentLeader (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)

-- | Representation of someone on a segment leaderboard.
data SegmentLeader = SegmentLeader
    { activityId     :: Integer
    , athleteGender  :: Maybe Char
    , athleteId      :: Integer
    , athleteName    :: Text
    , athleteProfile :: Text
    , averageHr      :: Double
    , averageWatts   :: Double
    , distance       :: Double
    , effortId       :: Integer
    , elapsedTime    :: Integer
    , movingTime     :: Integer
    , rank           :: Integer
    , startDate      :: UTCTime
    , startDateLocal :: UTCTime
    } deriving Show

instance FromJSON SegmentLeader where
    parseJSON (Object o) = SegmentLeader
        <$> o .: "activity_id"
        <*> o .: "athlete_gender"
        <*> o .: "athlete_id"
        <*> o .: "athlete_name"
        <*> o .: "athlete_profile"
        <*> o .: "average_hr"
        <*> o .: "average_watts"
        <*> o .: "distance"
        <*> o .: "effort_id"
        <*> o .: "elapsed_time"
        <*> o .: "moving_time"
        <*> o .: "rank"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
    parseJSON _ = empty
