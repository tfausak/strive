{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#summary>
module Scurry.Objects.Activities.ActivitySummary
    ( ActivitySummary (..)
    ) where

import           Control.Applicative      (empty, (<$>), (<*>))
import           Data.Aeson               (FromJSON, Value (Object), parseJSON,
                                           (.:))
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           Scurry.Objects.Athletes  (AthleteSummary)
import           Scurry.Objects.Polylines (PolylineSummary)

-- | Summary representation of an activity.
data ActivitySummary = ActivitySummary
    { achievementCount   :: Integer
    , athlete            :: AthleteSummary
    , athleteCount       :: Integer
    , averageSpeed       :: Double
    , averageWatts       :: Double
    , commentCount       :: Integer
    , commute            :: Bool
    , distance           :: Double
    , elapsedTime        :: Integer
    , endLatlng          :: (Double, Double)
    , externalId         :: Text
    , flagged            :: Bool
    , gearId             :: Text
    , hasKudoed          :: Bool
    , id                 :: Integer
    , kilojoules         :: Double
    , kudosCount         :: Integer
    , locationCity       :: Text
    , locationCountry    :: Text
    , locationState      :: Text
    , manual             :: Bool
    , map                :: PolylineSummary
    , maxSpeed           :: Double
    , movingTime         :: Integer
    , name               :: Text
    , photoCount         :: Integer
    , private            :: Bool
    , resourceState      :: Integer
    , startDate          :: UTCTime
    , startDateLocal     :: UTCTime
    , startLatitude      :: Double
    , startLatlng        :: (Double, Double)
    , startLongitude     :: Double
    , timezone           :: Text
    , totalElevationGain :: Double
    , trainer            :: Bool
    , type_              :: Text
    , uploadId           :: Integer
    } deriving Show

instance FromJSON ActivitySummary where
    parseJSON (Object o) = ActivitySummary
        <$> o .: "achievement_count"
        <*> o .: "athlete"
        <*> o .: "athlete_count"
        <*> o .: "average_speed"
        <*> o .: "average_watts"
        <*> o .: "comment_count"
        <*> o .: "commute"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .: "end_latlng"
        <*> o .: "external_id"
        <*> o .: "flagged"
        <*> o .: "gear_id"
        <*> o .: "has_kudoed"
        <*> o .: "id"
        <*> o .: "kilojoules"
        <*> o .: "kudos_count"
        <*> o .: "location_city"
        <*> o .: "location_country"
        <*> o .: "location_state"
        <*> o .: "manual"
        <*> o .: "map"
        <*> o .: "max_speed"
        <*> o .: "moving_time"
        <*> o .: "name"
        <*> o .: "photo_count"
        <*> o .: "private"
        <*> o .: "resource_state"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
        <*> o .: "start_latitude"
        <*> o .: "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "timezone"
        <*> o .: "total_elevation_gain"
        <*> o .: "trainer"
        <*> o .: "type"
        <*> o .: "upload_id"
    parseJSON _ = empty
