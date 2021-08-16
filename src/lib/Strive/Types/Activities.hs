{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | <http://strava.github.io/api/v3/activities/>
module Strive.Types.Activities
  ( ActivityDetailed (..)
  , ActivitySummary (..)
  , ActivityZoneDetailed (..)
  , ActivityZoneDistributionBucket (..)
  , ActivityLapSummary (..)
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Aeson.TH (deriveFromJSON)
import Data.Aeson.Types (Parser, withObject)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (ActivityType, ActivityZoneType, ResourceState)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteMeta)
import Strive.Types.Efforts (EffortDetailed)
import Strive.Types.Gear (GearSummary)
import Strive.Types.Polylines (PolylineDetailed, PolylineSummary)

parseLatlng :: Maybe [Double] -> Parser (Maybe (Double, Double))
parseLatlng = \case
  Nothing -> pure Nothing
  Just [] -> pure Nothing
  Just [lat, lng] -> pure $ Just (lat, lng)
  _ -> fail "Invalid array length when parsing a Latlng"

-- | <http://strava.github.io/api/v3/activities/#detailed>
data ActivityDetailed = ActivityDetailed
  { activityDetailed_achievementCount      :: Integer
  , activityDetailed_athlete               :: AthleteMeta
  , activityDetailed_athleteCount          :: Integer
  , activityDetailed_averageSpeed          :: Double
  , activityDetailed_averageWatts          :: Maybe Double
  , activityDetailed_averageHeartrate      :: Maybe Double
  , activityDetailed_calories              :: Double
  , activityDetailed_commentCount          :: Integer
  , activityDetailed_commute               :: Bool
  , activityDetailed_description           :: Maybe Text
  , activityDetailed_deviceWatts           :: Maybe Bool
  , activityDetailed_distance              :: Double
  , activityDetailed_elapsedTime           :: Integer
  , activityDetailed_endLatlng             :: Maybe (Double, Double)
  , activityDetailed_externalId            :: Maybe Text
  , activityDetailed_flagged               :: Bool
  , activityDetailed_gear                  :: GearSummary
  , activityDetailed_gearId                :: Maybe Text
  , activityDetailed_hasKudoed             :: Bool
  , activityDetailed_id                    :: Integer
  , activityDetailed_instagramPrimaryPhoto :: Maybe Text
  , activityDetailed_kilojoules            :: Maybe Double
  , activityDetailed_locationCity          :: Maybe Text
  , activityDetailed_locationCountry       :: Maybe Text
  , activityDetailed_locationState         :: Maybe Text
  , activityDetailed_manual                :: Bool
  , activityDetailed_map                   :: PolylineDetailed
  , activityDetailed_maxHeartrate          :: Maybe Double
  , activityDetailed_maxSpeed              :: Double
  , activityDetailed_movingTime            :: Integer
  , activityDetailed_name                  :: Text
  , activityDetailed_photoCount            :: Integer
  , activityDetailed_private               :: Bool
  , activityDetailed_resourceState         :: ResourceState
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
  , activityDetailed_type                  :: ActivityType
  , activityDetailed_uploadId              :: Maybe Integer
  , activityDetailed_weightedAverageWatts  :: Maybe Integer
  } deriving Show

instance FromJSON ActivityDetailed where
  parseJSON = withObject "ActivityDetailed" $ \v -> ActivityDetailed
    <$> v .: "achievement_count"
    <*> v .: "athlete"
    <*> v .: "athlete_count"
    <*> v .: "average_speed"
    <*> v .:? "average_watts"
    <*> v .:? "average_heartrate"
    <*> v .: "calories"
    <*> v .: "comment_count"
    <*> v .: "commute"
    <*> v .:? "description"
    <*> v .:? "device_watts"
    <*> v .: "distance"
    <*> v .: "elapsed_time"
    <*> (v .: "end_latlng" >>= parseLatlng)
    <*> v .:? "external_id"
    <*> v .: "flagged"
    <*> v .: "gear"
    <*> v .:? "gear_id"
    <*> v .: "has_kudoed"
    <*> v .: "id"
    <*> v .:? "instagram_primary_photo"
    <*> v .:? "kilojoules"
    <*> v .:? "location_city"
    <*> v .:? "location_country"
    <*> v .:? "location_state"
    <*> v .: "manual"
    <*> v .: "map"
    <*> v .:? "max_heartrate"
    <*> v .: "max_speed"
    <*> v .: "moving_time"
    <*> v .: "name"
    <*> v .: "photo_count"
    <*> v .: "private"
    <*> v .: "resource_state"
    <*> v .: "segment_efforts"
    <*> v .: "start_date"
    <*> v .: "start_date_local"
    <*> v .: "start_latitude"
    <*> (v .: "start_latlng" >>= parseLatlng)
    <*> v .: "start_longitude"
    <*> v .: "timezone"
    <*> v .: "total_elevation_gain"
    <*> v .: "trainer"
    <*> v .: "truncated"
    <*> v .: "type"
    <*> v .:? "upload_id"
    <*> v .:? "weighted_average_watts"

-- | <http://strava.github.io/api/v3/activities/#summary>
data ActivitySummary = ActivitySummary
  { activitySummary_achievementCount     :: Integer
  , activitySummary_athlete              :: AthleteMeta
  , activitySummary_athleteCount         :: Integer
  , activitySummary_averageSpeed         :: Double
  , activitySummary_averageWatts         :: Maybe Double
  , activitySummary_averageHeartrate     :: Maybe Double
  , activitySummary_commentCount         :: Integer
  , activitySummary_commute              :: Bool
  , activitySummary_deviceWatts          :: Maybe Bool
  , activitySummary_distance             :: Double
  , activitySummary_elapsedTime          :: Integer
  , activitySummary_endLatlng            :: Maybe (Double, Double)
  , activitySummary_externalId           :: Maybe Text
  , activitySummary_flagged              :: Bool
  , activitySummary_gearId               :: Maybe Text
  , activitySummary_hasKudoed            :: Bool
  , activitySummary_id                   :: Integer
  , activitySummary_kilojoules           :: Maybe Double
  , activitySummary_kudosCount           :: Integer
  , activitySummary_locationCity         :: Maybe Text
  , activitySummary_locationCountry      :: Maybe Text
  , activitySummary_locationState        :: Maybe Text
  , activitySummary_manual               :: Bool
  , activitySummary_map                  :: PolylineSummary
  , activitySummary_maxHeartrate         :: Maybe Double
  , activitySummary_maxSpeed             :: Double
  , activitySummary_movingTime           :: Integer
  , activitySummary_name                 :: Text
  , activitySummary_photoCount           :: Integer
  , activitySummary_private              :: Bool
  , activitySummary_resourceState        :: ResourceState
  , activitySummary_startDate            :: UTCTime
  , activitySummary_startDateLocal       :: UTCTime
  , activitySummary_startLatitude        :: Double
  , activitySummary_startLatlng          :: Maybe (Double, Double)
  , activitySummary_startLongitude       :: Double
  , activitySummary_timezone             :: Text
  , activitySummary_totalElevationGain   :: Double
  , activitySummary_trainer              :: Bool
  , activitySummary_type                 :: ActivityType
  , activitySummary_uploadId             :: Maybe Integer
  , activitySummary_weightedAverageWatts :: Maybe Integer
  } deriving Show

instance FromJSON ActivitySummary where
  parseJSON = withObject "ActivitySummary" $ \v -> ActivitySummary
    <$> v .: "achievement_count"
    <*> v .: "athlete"
    <*> v .: "athlete_count"
    <*> v .: "average_speed"
    <*> v .:? "average_watts"
    <*> v .:? "average_heartrate"
    <*> v .: "comment_count"
    <*> v .: "commute"
    <*> v .:? "device_watts"
    <*> v .: "distance"
    <*> v .: "elapsed_time"
    <*> (v .:? "end_latlng" >>= parseLatlng)
    <*> v .:? "external_id"
    <*> v .: "flagged"
    <*> v .:? "gear_id"
    <*> v .: "has_kudoed"
    <*> v .: "id"
    <*> v .:? "kilojoules"
    <*> v .: "kudos_count"
    <*> v .:? "location_city"
    <*> v .:? "location_country"
    <*> v .:? "location_state"
    <*> v .: "manual"
    <*> v .: "map"
    <*> v .:? "max_heartrate"
    <*> v .: "max_speed"
    <*> v .: "moving_time"
    <*> v .: "name"
    <*> v .: "photo_count"
    <*> v .: "private"
    <*> v .: "resource_state"
    <*> v .: "start_date"
    <*> v .: "start_date_local"
    <*> v .: "start_latitude"
    <*> (v .:? "start_latlng" >>= parseLatlng)
    <*> v .: "start_longitude"
    <*> v .: "timezone"
    <*> v .: "total_elevation_gain"
    <*> v .: "trainer"
    <*> v .: "type"
    <*> v .:? "upload_id"
    <*> v .:? "weighted_average_watts"

-- | <http://strava.github.io/api/v3/activities/#zones>
data ActivityZoneDistributionBucket = ActivityZoneDistributionBucket
  { activityZoneDistributionBucket_max  :: Integer
  , activityZoneDistributionBucket_min  :: Integer
  , activityZoneDistributionBucket_time :: Integer
  } deriving Show

$(deriveFromJSON options ''ActivityZoneDistributionBucket)

-- | <http://strava.github.io/api/v3/activities/#zones>
data ActivityZoneDetailed = ActivityZoneDetailed
  { activityZoneDetailed_distributionBuckets :: [ActivityZoneDistributionBucket]
  , activityZoneDetailed_resourceState       :: ResourceState
  , activityZoneDetailed_sensorBased         :: Bool
  , activityZoneDetailed_type                :: ActivityZoneType
  } deriving Show

$(deriveFromJSON options ''ActivityZoneDetailed)

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
    , activityLapSummary_resourceState      :: ResourceState
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
