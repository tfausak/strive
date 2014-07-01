{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/>
module Strive.Objects.Activities
    ( ActivityDetailed (..)
    , ActivitySummary (..)
    , ActivityZoneDetailed (..)
    , ActivityLapSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Athletes (AthleteMeta)
import Strive.Objects.Buckets (BucketSummary)
import Strive.Objects.Efforts (EffortSummary)
import Strive.Objects.Gear (GearSummary)
import Strive.Objects.Polylines (PolylineDetailed, PolylineSummary)

-- | <http://strava.github.io/api/v3/activities/#detailed>
data ActivityDetailed = ActivityDetailed
    { activityDetailedAchievementCount      :: Integer
    , activityDetailedAthlete               :: AthleteMeta
    , activityDetailedAthleteCount          :: Integer
    , activityDetailedAverageSpeed          :: Double
    , activityDetailedAverageWatts          :: Maybe Double
    , activityDetailedCalories              :: Double
    , activityDetailedCommentCount          :: Integer
    , activityDetailedCommute               :: Bool
    , activityDetailedDescription           :: Text
    , activityDetailedDistance              :: Double
    , activityDetailedElapsedTime           :: Integer
    , activityDetailedEndLatlng             :: Maybe (Double, Double)
    , activityDetailedExternalId            :: Maybe Text
    , activityDetailedFlagged               :: Bool
    , activityDetailedGear                  :: GearSummary
    , activityDetailedGearId                :: Maybe Text
    , activityDetailedHasKudoed             :: Bool
    , activityDetailedId                    :: Integer
    , activityDetailedInstagramPrimaryPhoto :: Text
    , activityDetailedKilojoules            :: Maybe Double
    , activityDetailedLocationCity          :: Maybe Text
    , activityDetailedLocationCountry       :: Text
    , activityDetailedLocationState         :: Maybe Text
    , activityDetailedManual                :: Bool
    , activityDetailedMap                   :: PolylineDetailed
    , activityDetailedMaxSpeed              :: Double
    , activityDetailedMovingTime            :: Integer
    , activityDetailedName                  :: Text
    , activityDetailedPhotoCount            :: Integer
    , activityDetailedPrivate               :: Bool
    , activityDetailedResourceState         :: Integer
    , activityDetailedSegmentEfforts        :: [EffortSummary]
    , activityDetailedStartDate             :: UTCTime
    , activityDetailedStartDateLocal        :: UTCTime
    , activityDetailedStartLatitude         :: Double
    , activityDetailedStartLatlng           :: Maybe (Double, Double)
    , activityDetailedStartLongitude        :: Double
    , activityDetailedTimezone              :: Text
    , activityDetailedTotalElevationGain    :: Double
    , activityDetailedTrainer               :: Bool
    , activityDetailedTruncated             :: Integer
    , activityDetailedType                  :: Text
    , activityDetailedUploadId              :: Maybe Integer
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
    { activitySummaryAchievementCount   :: Integer
    , activitySummaryAthlete            :: AthleteMeta
    , activitySummaryAthleteCount       :: Integer
    , activitySummaryAverageSpeed       :: Double
    , activitySummaryAverageWatts       :: Maybe Double
    , activitySummaryCommentCount       :: Integer
    , activitySummaryCommute            :: Bool
    , activitySummaryDistance           :: Double
    , activitySummaryElapsedTime        :: Integer
    , activitySummaryEndLatlng          :: Maybe (Double, Double)
    , activitySummaryExternalId         :: Maybe Text
    , activitySummaryFlagged            :: Bool
    , activitySummaryGearId             :: Maybe Text
    , activitySummaryHasKudoed          :: Bool
    , activitySummaryId                 :: Integer
    , activitySummaryKilojoules         :: Maybe Double
    , activitySummaryKudosCount         :: Integer
    , activitySummaryLocationCity       :: Maybe Text
    , activitySummaryLocationCountry    :: Text
    , activitySummaryLocationState      :: Maybe Text
    , activitySummaryManual             :: Bool
    , activitySummaryMap                :: PolylineSummary
    , activitySummaryMaxSpeed           :: Double
    , activitySummaryMovingTime         :: Integer
    , activitySummaryName               :: Text
    , activitySummaryPhotoCount         :: Integer
    , activitySummaryPrivate            :: Bool
    , activitySummaryResourceState      :: Integer
    , activitySummaryStartDate          :: UTCTime
    , activitySummaryStartDateLocal     :: UTCTime
    , activitySummaryStartLatitude      :: Double
    , activitySummaryStartLatlng        :: Maybe (Double, Double)
    , activitySummaryStartLongitude     :: Double
    , activitySummaryTimezone           :: Text
    , activitySummaryTotalElevationGain :: Double
    , activitySummaryTrainer            :: Bool
    , activitySummaryType               :: Text
    , activitySummaryUploadId           :: Maybe Integer
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
    { activityZoneDetailedDistributionBuckets :: [BucketSummary]
    , activityZoneDetailedResourceState       :: Integer
    , activityZoneDetailedSensorBased         :: Bool
    , activityZoneDetailedType                :: Text
    } deriving Show

instance FromJSON ActivityZoneDetailed where
    parseJSON (Object o) = ActivityZoneDetailed
        <$> o .: "distribution_buckets"
        <*> o .: "resource_state"
        <*> o .: "sensor_based"
        <*> o .: "type"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#laps>
data ActivityLapSummary = ActivityLapSummary
    { activityLapSummaryActivityId         :: Integer
    , activityLapSummaryAthleteId          :: Integer
    , activityLapSummaryAverageSpeed       :: Double
    , activityLapSummaryAverageWatts       :: Double
    , activityLapSummaryDistance           :: Double
    , activityLapSummaryElapsedTime        :: Integer
    , activityLapSummaryEndIndex           :: Integer
    , activityLapSummaryId                 :: Integer
    , activityLapSummaryLapIndex           :: Integer
    , activityLapSummaryMaxSpeed           :: Double
    , activityLapSummaryMovingTime         :: Double
    , activityLapSummaryName               :: Text
    , activityLapSummaryResourceState      :: Integer
    , activityLapSummaryStartDate          :: UTCTime
    , activityLapSummaryStartDateLocal     :: UTCTime
    , activityLapSummaryStartIndex         :: Integer
    , activityLapSummaryTotalElevationGain :: Double
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
