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
    { activityDetailed_achievementCount      :: Integer
    , activityDetailed_athlete               :: AthleteMeta
    , activityDetailed_athleteCount          :: Integer
    , activityDetailed_averageSpeed          :: Double
    , activityDetailed_averageWatts          :: Maybe Double
    , activityDetailed_calories              :: Double
    , activityDetailed_commentCount          :: Integer
    , activityDetailed_commute               :: Bool
    , activityDetailed_description           :: Text
    , activityDetailed_distance              :: Double
    , activityDetailed_elapsedTime           :: Integer
    , activityDetailed_endLatlng             :: Maybe (Double, Double)
    , activityDetailed_externalId            :: Maybe Text
    , activityDetailed_flagged               :: Bool
    , activityDetailed_gear                  :: GearSummary
    , activityDetailed_gearId                :: Maybe Text
    , activityDetailed_hasKudoed             :: Bool
    , activityDetailed_id                    :: Integer
    , activityDetailed_instagramPrimaryPhoto :: Text
    , activityDetailed_kilojoules            :: Maybe Double
    , activityDetailed_locationCity          :: Maybe Text
    , activityDetailed_locationCountry       :: Text
    , activityDetailed_locationState         :: Maybe Text
    , activityDetailed_manual                :: Bool
    , activityDetailed_map                   :: PolylineDetailed
    , activityDetailed_maxSpeed              :: Double
    , activityDetailed_movingTime            :: Integer
    , activityDetailed_name                  :: Text
    , activityDetailed_photoCount            :: Integer
    , activityDetailed_private               :: Bool
    , activityDetailed_resourceState         :: Integer
    , activityDetailed_segmentEfforts        :: [EffortDetailed]
    , activityDetailed_startDate             :: UTCTime
    , activityDetailed_startDateLocal        :: UTCTime
    , activityDetailed_startLatitude         :: Double
    , activityDetailed_startLatlng           :: Maybe (Double, Double)
    , activityDetailed_startLongitude        :: Double
    , activityDetailed_timezone              :: Text
    , activityDetailed_totalElevationGain    :: Double
    , activityDetailed_trainer               :: Bool
    , activityDetailed_truncated             :: Integer
    , activityDetailed_type                  :: Text
    , activityDetailed_uploadId              :: Maybe Integer
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
    { activitySummary_achievementCount   :: Integer
    , activitySummary_athlete            :: AthleteMeta
    , activitySummary_athleteCount       :: Integer
    , activitySummary_averageSpeed       :: Double
    , activitySummary_averageWatts       :: Maybe Double
    , activitySummary_commentCount       :: Integer
    , activitySummary_commute            :: Bool
    , activitySummary_distance           :: Double
    , activitySummary_elapsedTime        :: Integer
    , activitySummary_endLatlng          :: Maybe (Double, Double)
    , activitySummary_externalId         :: Maybe Text
    , activitySummary_flagged            :: Bool
    , activitySummary_gearId             :: Maybe Text
    , activitySummary_hasKudoed          :: Bool
    , activitySummary_id                 :: Integer
    , activitySummary_kilojoules         :: Maybe Double
    , activitySummary_kudosCount         :: Integer
    , activitySummary_locationCity       :: Maybe Text
    , activitySummary_locationCountry    :: Text
    , activitySummary_locationState      :: Maybe Text
    , activitySummary_manual             :: Bool
    , activitySummary_map                :: PolylineSummary
    , activitySummary_maxSpeed           :: Double
    , activitySummary_movingTime         :: Integer
    , activitySummary_name               :: Text
    , activitySummary_photoCount         :: Integer
    , activitySummary_private            :: Bool
    , activitySummary_resourceState      :: Integer
    , activitySummary_startDate          :: UTCTime
    , activitySummary_startDateLocal     :: UTCTime
    , activitySummary_startLatitude      :: Double
    , activitySummary_startLatlng        :: Maybe (Double, Double)
    , activitySummary_startLongitude     :: Double
    , activitySummary_timezone           :: Text
    , activitySummary_totalElevationGain :: Double
    , activitySummary_trainer            :: Bool
    , activitySummary_type               :: Text
    , activitySummary_uploadId           :: Maybe Integer
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
    { activityZoneDetailed_distributionBuckets :: [ActivityZoneDistributionBucket]
    , activityZoneDetailed_resourceState       :: Integer
    , activityZoneDetailed_sensorBased         :: Bool
    , activityZoneDetailed_type                :: Text
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
    { activityZoneDistributionBucket_max  :: Integer
    , activityZoneDistributionBucket_min  :: Integer
    , activityZoneDistributionBucket_time :: Integer
    } deriving Show

instance FromJSON ActivityZoneDistributionBucket where
    parseJSON (Object o) = ActivityZoneDistributionBucket
        <$> o .: "max"
        <*> o .: "min"
        <*> o .: "time"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#laps>
data ActivityLapSummary = ActivityLapSummary
    { activityLapSummary_activityId         :: Integer
    , activityLapSummary_athleteId          :: Integer
    , activityLapSummary_averageSpeed       :: Double
    , activityLapSummary_averageWatts       :: Double
    , activityLapSummary_distance           :: Double
    , activityLapSummary_elapsedTime        :: Integer
    , activityLapSummary_endIndex           :: Integer
    , activityLapSummary_id                 :: Integer
    , activityLapSummary_lapIndex           :: Integer
    , activityLapSummary_maxSpeed           :: Double
    , activityLapSummary_movingTime         :: Double
    , activityLapSummary_name               :: Text
    , activityLapSummary_resourceState      :: Integer
    , activityLapSummary_startDate          :: UTCTime
    , activityLapSummary_startDateLocal     :: UTCTime
    , activityLapSummary_startIndex         :: Integer
    , activityLapSummary_totalElevationGain :: Double
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
