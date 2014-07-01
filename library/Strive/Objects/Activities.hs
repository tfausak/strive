{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/>
module Strive.Objects.Activities
    ( ActivityDetailed (..)
    , ActivitySummary (..)
    , ActivityZoneDetailed (..)
    , ActivityZoneDistributionBucket (..)
    , ActivityLapSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Athletes (AthleteMeta)
import Strive.Objects.Efforts (EffortDetailed)
import Strive.Objects.Gear (GearSummary)
import Strive.Objects.Polylines (PolylineDetailed, PolylineSummary)

-- | <http://strava.github.io/api/v3/activities/#detailed>
data ActivityDetailed = ActivityDetailed
    { _activityDetailed_achievementCount      :: Integer
    , _activityDetailed_athlete               :: AthleteMeta
    , _activityDetailed_athleteCount          :: Integer
    , _activityDetailed_averageSpeed          :: Double
    , _activityDetailed_averageWatts          :: Maybe Double
    , _activityDetailed_calories              :: Double
    , _activityDetailed_commentCount          :: Integer
    , _activityDetailed_commute               :: Bool
    , _activityDetailed_description           :: Text
    , _activityDetailed_distance              :: Double
    , _activityDetailed_elapsedTime           :: Integer
    , _activityDetailed_endLatlng             :: Maybe (Double, Double)
    , _activityDetailed_externalId            :: Maybe Text
    , _activityDetailed_flagged               :: Bool
    , _activityDetailed_gear                  :: GearSummary
    , _activityDetailed_gearId                :: Maybe Text
    , _activityDetailed_hasKudoed             :: Bool
    , _activityDetailed_id                    :: Integer
    , _activityDetailed_instagramPrimaryPhoto :: Text
    , _activityDetailed_kilojoules            :: Maybe Double
    , _activityDetailed_locationCity          :: Maybe Text
    , _activityDetailed_locationCountry       :: Text
    , _activityDetailed_locationState         :: Maybe Text
    , _activityDetailed_manual                :: Bool
    , _activityDetailed_map                   :: PolylineDetailed
    , _activityDetailed_maxSpeed              :: Double
    , _activityDetailed_movingTime            :: Integer
    , _activityDetailed_name                  :: Text
    , _activityDetailed_photoCount            :: Integer
    , _activityDetailed_private               :: Bool
    , _activityDetailed_resourceState         :: Integer
    , _activityDetailed_segmentEfforts        :: [EffortDetailed]
    , _activityDetailed_startDate             :: UTCTime
    , _activityDetailed_startDateLocal        :: UTCTime
    , _activityDetailed_startLatitude         :: Double
    , _activityDetailed_startLatlng           :: Maybe (Double, Double)
    , _activityDetailed_startLongitude        :: Double
    , _activityDetailed_timezone              :: Text
    , _activityDetailed_totalElevationGain    :: Double
    , _activityDetailed_trainer               :: Bool
    , _activityDetailed_truncated             :: Integer
    , _activityDetailed_type                  :: Text
    , _activityDetailed_uploadId              :: Maybe Integer
    } deriving Show

instance FromJSON ActivityDetailed where
    parseJSON (Object o) = ActivityDetailed
        <$> o .: "achievement_count"
        <*> o .: "athlete"
        <*> o .: "athlete_count"
        <*> o .: "average_speed"
        <*> o .:? "average_watts"
        <*> o .: "calories"
        <*> o .: "comment_count"
        <*> o .: "commute"
        <*> o .: "description"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .:? "end_latlng"
        <*> o .:? "external_id"
        <*> o .: "flagged"
        <*> o .: "gear"
        <*> o .:? "gear_id"
        <*> o .: "has_kudoed"
        <*> o .: "id"
        <*> o .: "instagram_primary_photo"
        <*> o .:? "kilojoules"
        <*> o .:? "location_city"
        <*> o .: "location_country"
        <*> o .:? "location_state"
        <*> o .: "manual"
        <*> o .: "map"
        <*> o .: "max_speed"
        <*> o .: "moving_time"
        <*> o .: "name"
        <*> o .: "photo_count"
        <*> o .: "private"
        <*> o .: "resource_state"
        <*> o .: "segment_efforts"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
        <*> o .: "start_latitude"
        <*> o .:? "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "timezone"
        <*> o .: "total_elevation_gain"
        <*> o .: "trainer"
        <*> o .: "truncated"
        <*> o .: "type"
        <*> o .:? "upload_id"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#summary>
data ActivitySummary = ActivitySummary
    { _activitySummary_achievementCount   :: Integer
    , _activitySummary_athlete            :: AthleteMeta
    , _activitySummary_athleteCount       :: Integer
    , _activitySummary_averageSpeed       :: Double
    , _activitySummary_averageWatts       :: Maybe Double
    , _activitySummary_commentCount       :: Integer
    , _activitySummary_commute            :: Bool
    , _activitySummary_distance           :: Double
    , _activitySummary_elapsedTime        :: Integer
    , _activitySummary_endLatlng          :: Maybe (Double, Double)
    , _activitySummary_externalId         :: Maybe Text
    , _activitySummary_flagged            :: Bool
    , _activitySummary_gearId             :: Maybe Text
    , _activitySummary_hasKudoed          :: Bool
    , _activitySummary_id                 :: Integer
    , _activitySummary_kilojoules         :: Maybe Double
    , _activitySummary_kudosCount         :: Integer
    , _activitySummary_locationCity       :: Maybe Text
    , _activitySummary_locationCountry    :: Text
    , _activitySummary_locationState      :: Maybe Text
    , _activitySummary_manual             :: Bool
    , _activitySummary_map                :: PolylineSummary
    , _activitySummary_maxSpeed           :: Double
    , _activitySummary_movingTime         :: Integer
    , _activitySummary_name               :: Text
    , _activitySummary_photoCount         :: Integer
    , _activitySummary_private            :: Bool
    , _activitySummary_resourceState      :: Integer
    , _activitySummary_startDate          :: UTCTime
    , _activitySummary_startDateLocal     :: UTCTime
    , _activitySummary_startLatitude      :: Double
    , _activitySummary_startLatlng        :: Maybe (Double, Double)
    , _activitySummary_startLongitude     :: Double
    , _activitySummary_timezone           :: Text
    , _activitySummary_totalElevationGain :: Double
    , _activitySummary_trainer            :: Bool
    , _activitySummary_type               :: Text
    , _activitySummary_uploadId           :: Maybe Integer
    } deriving Show

instance FromJSON ActivitySummary where
    parseJSON (Object o) = ActivitySummary
        <$> o .: "achievement_count"
        <*> o .: "athlete"
        <*> o .: "athlete_count"
        <*> o .: "average_speed"
        <*> o .:? "average_watts"
        <*> o .: "comment_count"
        <*> o .: "commute"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .:? "end_latlng"
        <*> o .:? "external_id"
        <*> o .: "flagged"
        <*> o .:? "gear_id"
        <*> o .: "has_kudoed"
        <*> o .: "id"
        <*> o .:? "kilojoules"
        <*> o .: "kudos_count"
        <*> o .:? "location_city"
        <*> o .: "location_country"
        <*> o .:? "location_state"
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
        <*> o .:? "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "timezone"
        <*> o .: "total_elevation_gain"
        <*> o .: "trainer"
        <*> o .: "type"
        <*> o .:? "upload_id"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#zones>
data ActivityZoneDetailed = ActivityZoneDetailed
    { _activityZoneDetailed_distributionBuckets :: [ActivityZoneDistributionBucket]
    , _activityZoneDetailed_resourceState       :: Integer
    , _activityZoneDetailed_sensorBased         :: Bool
    , _activityZoneDetailed_type                :: Text
    } deriving Show

instance FromJSON ActivityZoneDetailed where
    parseJSON (Object o) = ActivityZoneDetailed
        <$> o .: "distribution_buckets"
        <*> o .: "resource_state"
        <*> o .: "sensor_based"
        <*> o .: "type"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#zones>
data ActivityZoneDistributionBucket = ActivityZoneDistributionBucket
    { _activityZoneDistributionBucket_max  :: Integer
    , _activityZoneDistributionBucket_min  :: Integer
    , _activityZoneDistributionBucket_time :: Integer
    } deriving Show

instance FromJSON ActivityZoneDistributionBucket where
    parseJSON (Object o) = ActivityZoneDistributionBucket
        <$> o .: "max"
        <*> o .: "min"
        <*> o .: "time"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#laps>
data ActivityLapSummary = ActivityLapSummary
    { _activityLapSummary_activityId         :: Integer
    , _activityLapSummary_athleteId          :: Integer
    , _activityLapSummary_averageSpeed       :: Double
    , _activityLapSummary_averageWatts       :: Double
    , _activityLapSummary_distance           :: Double
    , _activityLapSummary_elapsedTime        :: Integer
    , _activityLapSummary_endIndex           :: Integer
    , _activityLapSummary_id                 :: Integer
    , _activityLapSummary_lapIndex           :: Integer
    , _activityLapSummary_maxSpeed           :: Double
    , _activityLapSummary_movingTime         :: Double
    , _activityLapSummary_name               :: Text
    , _activityLapSummary_resourceState      :: Integer
    , _activityLapSummary_startDate          :: UTCTime
    , _activityLapSummary_startDateLocal     :: UTCTime
    , _activityLapSummary_startIndex         :: Integer
    , _activityLapSummary_totalElevationGain :: Double
    } deriving Show

instance FromJSON ActivityLapSummary where
    parseJSON (Object o) = ActivityLapSummary
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
