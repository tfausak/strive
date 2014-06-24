{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#laps>
module Scurry.Objects.Efforts.EffortLap
    ( EffortLap (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)

-- | Representation of a lap effort.
data EffortLap = EffortLap
    { activityId         :: Integer
    , athleteId          :: Integer
    , averageSpeed       :: Double
    , averageWatts       :: Double
    , distance           :: Double
    , elapsedTime        :: Integer
    , endIndex           :: Integer
    , id                 :: Integer
    , lapIndex           :: Integer
    , maxSpeed           :: Double
    , movingTime         :: Double
    , name               :: Text
    , resourceState      :: Integer
    , startDate          :: UTCTime
    , startDateLocal     :: UTCTime
    , startIndex         :: Integer
    , totalElevationGain :: Double
    } deriving Show

instance FromJSON EffortLap where
    parseJSON (Object o) = EffortLap
        <$> ((o .: "activity") >>= (.: "id"))
        <*> ((o .: "athlete") >>= (.: "id"))
        <*> o .: "average_speed"
        <*> o .: "average_watts"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .: "end_index"
        <*> o .: "id"
        <*> o .: "lap_index"
        <*> o .: "max_speed"
        <*> o .: "moving_time"
        <*> o .: "name"
        <*> o .: "resource_state"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
        <*> o .: "start_index"
        <*> o .: "total_elevation_gain"
    parseJSON _ = empty
