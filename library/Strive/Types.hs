{-# LANGUAGE OverloadedStrings #-}

-- | Data types representing responses from the API.
module Strive.Types where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import GPolyline (decodeline)
import Strive.Enums

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#example-response>
data TokenExchangeResponse = TokenExchangeResponse
  { tokenExchangeResponse_accessToken :: Text
  , tokenExchangeResponse_athlete     :: AthleteDetailed
  } deriving Show

instance FromJSON TokenExchangeResponse where
  parseJSON (Object o) = TokenExchangeResponse
    <$> o .: "access_token"
    <*> o .: "athlete"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/oauth/#example-response-1>
data DeauthorizationResponse = DeauthorizationResponse
  { deauthorizationResponse_accessToken :: Text
  } deriving Show

instance FromJSON DeauthorizationResponse where
  parseJSON (Object o) = DeauthorizationResponse
    <$> o .: "access_token"
  parseJSON _ = empty

-- * Athletes

-- | <http://strava.github.io/api/v3/athlete/#detailed>
data AthleteDetailed = AthleteDetailed
  { athleteDetailed_bikes                 :: [GearSummary]
  , athleteDetailed_city                  :: Text
  , athleteDetailed_clubs                 :: [ClubSummary]
  , athleteDetailed_country               :: Text
  , athleteDetailed_createdAt             :: UTCTime
  , athleteDetailed_datePreference        :: Text
  , athleteDetailed_email                 :: Text
  , athleteDetailed_firstname             :: Text
  , athleteDetailed_follower              :: Maybe Text
  , athleteDetailed_followerCount         :: Integer
  , athleteDetailed_friend                :: Maybe Text
  , athleteDetailed_friendCount           :: Integer
  , athleteDetailed_ftp                   :: Maybe Integer
  , athleteDetailed_id                    :: Integer
  , athleteDetailed_lastname              :: Text
  , athleteDetailed_measurementPreference :: MeasurementPreference
  , athleteDetailed_mutualFriendCount     :: Integer
  , athleteDetailed_premium               :: Bool
  , athleteDetailed_profile               :: Text
  , athleteDetailed_profileMedium         :: Text
  , athleteDetailed_resourceState         :: ResourceState
  , athleteDetailed_sex                   :: Maybe Gender
  , athleteDetailed_shoes                 :: [GearSummary]
  , athleteDetailed_state                 :: Text
  , athleteDetailed_updatedAt             :: UTCTime
  } deriving Show

instance FromJSON AthleteDetailed where
  parseJSON (Object o) = AthleteDetailed
    <$> o .: "bikes"
    <*> o .: "city"
    <*> o .: "clubs"
    <*> o .: "country"
    <*> o .: "created_at"
    <*> o .: "date_preference"
    <*> o .: "email"
    <*> o .: "firstname"
    <*> o .:? "follower"
    <*> o .: "follower_count"
    <*> o .:? "friend"
    <*> o .: "friend_count"
    <*> o .:? "ftp"
    <*> o .: "id"
    <*> o .: "lastname"
    <*> o .: "measurement_preference"
    <*> o .: "mutual_friend_count"
    <*> o .: "premium"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .:? "sex"
    <*> o .: "shoes"
    <*> o .: "state"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/athlete/#summary>
data AthleteSummary = AthleteSummary
  { athleteSummary_city          :: Maybe Text
  , athleteSummary_country       :: Maybe Text
  , athleteSummary_createdAt     :: UTCTime
  , athleteSummary_firstname     :: Text
  , athleteSummary_follower      :: Maybe Text
  , athleteSummary_friend        :: Maybe Text
  , athleteSummary_id            :: Integer
  , athleteSummary_lastname      :: Text
  , athleteSummary_premium       :: Bool
  , athleteSummary_profile       :: Text
  , athleteSummary_profileMedium :: Text
  , athleteSummary_resourceState :: ResourceState
  , athleteSummary_sex           :: Maybe Gender
  , athleteSummary_state         :: Text
  , athleteSummary_updatedAt     :: UTCTime
  } deriving Show

instance FromJSON AthleteSummary where
  parseJSON (Object o) = AthleteSummary
    <$> o .:? "city"
    <*> o .:? "country"
    <*> o .: "created_at"
    <*> o .: "firstname"
    <*> o .:? "follower"
    <*> o .:? "friend"
    <*> o .: "id"
    <*> o .: "lastname"
    <*> o .: "premium"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .:? "sex"
    <*> o .: "state"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/athlete/#meta>
data AthleteMeta = AthleteMeta
  { athleteMeta_id            :: Integer
  , athleteMeta_resourceState :: ResourceState
  } deriving Show

instance FromJSON AthleteMeta where
  parseJSON (Object o) = AthleteMeta
    <$> o .: "id"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- * Activities

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
  , activityDetailed_description           :: Maybe Text
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
  , activityDetailed_locationCountry       :: Text
  , activityDetailed_locationState         :: Maybe Text
  , activityDetailed_manual                :: Bool
  , activityDetailed_map                   :: PolylineDetailed
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
    <*> o .:? "description"
    <*> o .: "distance"
    <*> o .: "elapsed_time"
    <*> o .:? "end_latlng"
    <*> o .:? "external_id"
    <*> o .: "flagged"
    <*> o .: "gear"
    <*> o .:? "gear_id"
    <*> o .: "has_kudoed"
    <*> o .: "id"
    <*> o .:? "instagram_primary_photo"
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
  , activitySummary_resourceState      :: ResourceState
  , activitySummary_startDate          :: UTCTime
  , activitySummary_startDateLocal     :: UTCTime
  , activitySummary_startLatitude      :: Double
  , activitySummary_startLatlng        :: Maybe (Double, Double)
  , activitySummary_startLongitude     :: Double
  , activitySummary_timezone           :: Text
  , activitySummary_totalElevationGain :: Double
  , activitySummary_trainer            :: Bool
  , activitySummary_type               :: ActivityType
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

-- | <http://strava.github.io/api/v3/activities/#detailed>
data PolylineDetailed = PolylineDetailed
  { polylineDetailed_id              :: Text
  , polylineDetailed_polyline        :: [(Double, Double)]
  , polylineDetailed_resourceState   :: ResourceState
  , polylineDetailed_summaryPolyline :: Maybe [(Double, Double)]
  } deriving Show

instance FromJSON PolylineDetailed where
  parseJSON (Object o) = do
    id <- o .: "id"
    polyline <- o .: "polyline"
    resourceState <- o .: "resource_state"
    summaryPolyline <- o .:? "summary_polyline"

    return PolylineDetailed
      { polylineDetailed_id = id
      , polylineDetailed_polyline = decodeline polyline
      , polylineDetailed_resourceState = resourceState
      , polylineDetailed_summaryPolyline = fmap decodeline summaryPolyline
      }

  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#summary>
data PolylineSummary = PolylineSummary
  { polylineSummary_id              :: Text
  , polylineSummary_resourceState   :: ResourceState
  , polylineSummary_summaryPolyline :: Maybe [(Double, Double)]
  } deriving Show

instance FromJSON PolylineSummary where
  parseJSON (Object o) = do
    id <- o .: "id"
    resourceState <- o .: "resource_state"
    summaryPolyline <- o .:? "summary_polyline"

    return PolylineSummary
      { polylineSummary_id = id
      , polylineSummary_resourceState = resourceState
      , polylineSummary_summaryPolyline = fmap decodeline summaryPolyline
      }

  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#zones>
data ActivityZoneDetailed = ActivityZoneDetailed
  { activityZoneDetailed_distributionBuckets :: [ActivityZoneDistributionBucket]
  , activityZoneDetailed_resourceState       :: ResourceState
  , activityZoneDetailed_sensorBased         :: Bool
  , activityZoneDetailed_type                :: ActivityZoneType
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

-- * Comments

-- | <http://strava.github.io/api/v3/comments/#summary-and-detailed-representation-attributes>
data CommentSummary = CommentSummary
  { commentSummary_activityId    :: Integer
  , commentSummary_athlete       :: AthleteSummary
  , commentSummary_createdAt     :: UTCTime
  , commentSummary_id            :: Integer
  , commentSummary_resourceState :: ResourceState
  , commentSummary_text          :: Text
  } deriving Show

instance FromJSON CommentSummary where
  parseJSON (Object o) = CommentSummary
    <$> o .: "activity_id"
    <*> o .: "athlete"
    <*> o .: "created_at"
    <*> o .: "id"
    <*> o .: "resource_state"
    <*> o .: "text"
  parseJSON _ = empty

-- * Photos

-- | <http://strava.github.io/api/v3/photos/#summary-and-detailed-representation-attributes>
data PhotoSummary = PhotoSummary
  { photoSummary_activityId    :: Integer
  , photoSummary_caption       :: Text
  , photoSummary_createdAt     :: UTCTime
  , photoSummary_id            :: Integer
  , photoSummary_location      :: Maybe (Double, Double)
  , photoSummary_ref           :: Text
  , photoSummary_resourceState :: ResourceState
  , photoSummary_type          :: PhotoType
  , photoSummary_uid           :: Text
  , photoSummary_uploadedAt    :: UTCTime
  } deriving Show

instance FromJSON PhotoSummary where
  parseJSON (Object o) = PhotoSummary
    <$> o .: "activity_id"
    <*> o .: "caption"
    <*> o .: "created_at"
    <*> o .: "id"
    <*> o .:? "location"
    <*> o .: "ref"
    <*> o .: "resource_state"
    <*> o .: "type"
    <*> o .: "uid"
    <*> o .: "uploaded_at"
  parseJSON _ = empty

-- * Clubs

-- | <http://strava.github.io/api/v3/clubs/#detailed>
data ClubDetailed = ClubDetailed
  { clubDetailed_city          :: Text
  , clubDetailed_clubType      :: Text
  , clubDetailed_country       :: Text
  , clubDetailed_description   :: Text
  , clubDetailed_id            :: Integer
  , clubDetailed_memberCount   :: Integer
  , clubDetailed_name          :: Text
  , clubDetailed_private       :: Bool
  , clubDetailed_profile       :: Text
  , clubDetailed_profileMedium :: Text
  , clubDetailed_resourceState :: ResourceState
  , clubDetailed_sportType     :: Text
  , clubDetailed_state         :: Text
  } deriving Show

instance FromJSON ClubDetailed where
  parseJSON (Object o) = ClubDetailed
    <$> o .: "city"
    <*> o .: "club_type"
    <*> o .: "country"
    <*> o .: "description"
    <*> o .: "id"
    <*> o .: "member_count"
    <*> o .: "name"
    <*> o .: "private"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .: "sport_type"
    <*> o .: "state"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/clubs/#summary>
data ClubSummary = ClubSummary
  { clubSummary_id            :: Integer
  , clubSummary_name          :: Text
  , clubSummary_profile       :: Text
  , clubSummary_profileMedium :: Text
  , clubSummary_resourceState :: ResourceState
  } deriving Show

instance FromJSON ClubSummary where
  parseJSON (Object o) = ClubSummary
    <$> o .: "id"
    <*> o .: "name"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- * Gear

-- | <http://strava.github.io/api/v3/gear/#detailed>
data GearDetailed = GearDetailed
  { gearDetailed_brandName     :: Text
  , gearDetailed_description   :: Text
  , gearDetailed_distance      :: Double
  , gearDetailed_frameType     :: Maybe Integer
  , gearDetailed_id            :: Text
  , gearDetailed_modelName     :: Text
  , gearDetailed_name          :: Text
  , gearDetailed_primary       :: Bool
  , gearDetailed_resourceState :: ResourceState
  } deriving Show

instance FromJSON GearDetailed where
  parseJSON (Object o) = GearDetailed
    <$> o .: "brand_name"
    <*> o .: "description"
    <*> o .: "distance"
    <*> o .:? "frame_type"
    <*> o .: "id"
    <*> o .: "model_name"
    <*> o .: "name"
    <*> o .: "primary"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/gear/#summary>
data GearSummary = GearSummary
  { gearSummary_distance      :: Double
  , gearSummary_id            :: Text
  , gearSummary_name          :: Text
  , gearSummary_primary       :: Bool
  , gearSummary_resourceState :: ResourceState
  } deriving Show

instance FromJSON GearSummary where
  parseJSON (Object o) = GearSummary
    <$> o .: "distance"
    <*> o .: "id"
    <*> o .: "name"
    <*> o .: "primary"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- * Segments

-- | <http://strava.github.io/api/v3/segments/#detailed>
data SegmentDetailed = SegmentDetailed
  { segmentDetailed_activityType       :: ActivityType
  , segmentDetailed_athleteCount       :: Integer
  , segmentDetailed_averageGrade       :: Double
  , segmentDetailed_city               :: Text
  , segmentDetailed_climbCategory      :: Integer
  , segmentDetailed_country            :: Text
  , segmentDetailed_createdAt          :: UTCTime
  , segmentDetailed_distance           :: Double
  , segmentDetailed_effortCount        :: Integer
  , segmentDetailed_elevationHigh      :: Double
  , segmentDetailed_elevationLow       :: Double
  , segmentDetailed_endLatitude        :: Double
  , segmentDetailed_endLatlng          :: (Double, Double)
  , segmentDetailed_endLongitude       :: Double
  , segmentDetailed_hazardous          :: Bool
  , segmentDetailed_id                 :: Integer
  , segmentDetailed_map                :: PolylineDetailed
  , segmentDetailed_maximumGrade       :: Double
  , segmentDetailed_name               :: Text
  , segmentDetailed_private            :: Bool
  , segmentDetailed_resourceState      :: ResourceState
  , segmentDetailed_starCount          :: Integer
  , segmentDetailed_starred            :: Bool
  , segmentDetailed_startLatitude      :: Double
  , segmentDetailed_startLatlng        :: (Double, Double)
  , segmentDetailed_startLongitude     :: Double
  , segmentDetailed_state              :: Text
  , segmentDetailed_totalElevationGain :: Double
  , segmentDetailed_updatedAt          :: UTCTime
  } deriving Show

instance FromJSON SegmentDetailed where
  parseJSON (Object o) = SegmentDetailed
    <$> o .: "activity_type"
    <*> o .: "athlete_count"
    <*> o .: "average_grade"
    <*> o .: "city"
    <*> o .: "climb_category"
    <*> o .: "country"
    <*> o .: "created_at"
    <*> o .: "distance"
    <*> o .: "effort_count"
    <*> o .: "elevation_high"
    <*> o .: "elevation_low"
    <*> o .: "end_latitude"
    <*> o .: "end_latlng"
    <*> o .: "end_longitude"
    <*> o .: "hazardous"
    <*> o .: "id"
    <*> o .: "map"
    <*> o .: "maximum_grade"
    <*> o .: "name"
    <*> o .: "private"
    <*> o .: "resource_state"
    <*> o .: "star_count"
    <*> o .: "starred"
    <*> o .: "start_latitude"
    <*> o .: "start_latlng"
    <*> o .: "start_longitude"
    <*> o .: "state"
    <*> o .: "total_elevation_gain"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#summary>
data SegmentSummary = SegmentSummary
  { segmentSummary_activityType   :: ActivityType
  , segmentSummary_averageGrade   :: Double
  , segmentSummary_city           :: Text
  , segmentSummary_climbCategory  :: Integer
  , segmentSummary_country        :: Text
  , segmentSummary_distance       :: Double
  , segmentSummary_elevationHigh  :: Double
  , segmentSummary_elevationLow   :: Double
  , segmentSummary_endLatitude    :: Double
  , segmentSummary_endLatlng      :: (Double, Double)
  , segmentSummary_endLongitude   :: Double
  , segmentSummary_id             :: Integer
  , segmentSummary_maximumGrade   :: Double
  , segmentSummary_name           :: Text
  , segmentSummary_private        :: Bool
  , segmentSummary_resourceState  :: ResourceState
  , segmentSummary_starred        :: Bool
  , segmentSummary_startLatitude  :: Double
  , segmentSummary_startLatlng    :: (Double, Double)
  , segmentSummary_startLongitude :: Double
  , segmentSummary_state          :: Text
  } deriving Show

instance FromJSON SegmentSummary where
  parseJSON (Object o) = SegmentSummary
    <$> o .: "activity_type"
    <*> o .: "average_grade"
    <*> o .: "city"
    <*> o .: "climb_category"
    <*> o .: "country"
    <*> o .: "distance"
    <*> o .: "elevation_high"
    <*> o .: "elevation_low"
    <*> o .: "end_latitude"
    <*> o .: "end_latlng"
    <*> o .: "end_longitude"
    <*> o .: "id"
    <*> o .: "maximum_grade"
    <*> o .: "name"
    <*> o .: "private"
    <*> o .: "resource_state"
    <*> o .: "starred"
    <*> o .: "start_latitude"
    <*> o .: "start_latlng"
    <*> o .: "start_longitude"
    <*> o .: "state"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
data SegmentLeaderboardResponse = SegmentLeaderboardResponse
  { segmentLeaderboard_effortCount :: Integer
  , segmentLeaderboard_entryCount  :: Integer
  , segmentLeaderboard_entries     :: [SegmentLeaderboardEntry]
  } deriving Show

instance FromJSON SegmentLeaderboardResponse where
  parseJSON (Object o) = SegmentLeaderboardResponse
    <$> o .: "effort_count"
    <*> o .: "entry_count"
    <*> o .: "entries"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
data SegmentLeaderboardEntry = SegmentLeaderboardEntry
  { segmentLeaderboardEntry_activityId     :: Integer
  , segmentLeaderboardEntry_athleteGender  :: Maybe Gender
  , segmentLeaderboardEntry_athleteId      :: Integer
  , segmentLeaderboardEntry_athleteName    :: Text
  , segmentLeaderboardEntry_athleteProfile :: Text
  , segmentLeaderboardEntry_averageHr      :: Double
  , segmentLeaderboardEntry_averageWatts   :: Double
  , segmentLeaderboardEntry_distance       :: Double
  , segmentLeaderboardEntry_effortId       :: Integer
  , segmentLeaderboardEntry_elapsedTime    :: Integer
  , segmentLeaderboardEntry_movingTime     :: Integer
  , segmentLeaderboardEntry_rank           :: Integer
  , segmentLeaderboardEntry_startDate      :: UTCTime
  , segmentLeaderboardEntry_startDateLocal :: UTCTime
  } deriving Show

instance FromJSON SegmentLeaderboardEntry where
  parseJSON (Object o) = SegmentLeaderboardEntry
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

-- | <http://strava.github.io/api/v3/segments/#explore>
data SegmentExplorerResponse = SegmentExplorerResponse
  { segmentExplorerResponse_entries :: [SegmentExplorerEntry]
  } deriving Show

instance FromJSON SegmentExplorerResponse where
  parseJSON (Object o) = SegmentExplorerResponse
    <$> o .: "segments"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#explore>
data SegmentExplorerEntry = SegmentExplorerEntry
  { segmentExplorerEntry_avgGrade          :: Double
  , segmentExplorerEntry_climbCategory     :: Integer
  , segmentExplorerEntry_climbCategoryDesc :: String
  , segmentExplorerEntry_distance          :: Double
  , segmentExplorerEntry_elevDifference    :: Double
  , segmentExplorerEntry_endLatlng         :: (Double, Double)
  , segmentExplorerEntry_id                :: Integer
  , segmentExplorerEntry_name              :: Text
  , segmentExplorerEntry_points            :: Text
  , segmentExplorerEntry_resourceState     :: ResourceState
  , segmentExplorerEntry_starred           :: Bool
  , segmentExplorerEntry_startLatlng       :: (Double, Double)
  } deriving Show

instance FromJSON SegmentExplorerEntry where
  parseJSON (Object o) = SegmentExplorerEntry
    <$> o .: "avg_grade"
    <*> o .: "climb_category"
    <*> o .: "climb_category_desc"
    <*> o .: "distance"
    <*> o .: "elev_difference"
    <*> o .: "end_latlng"
    <*> o .: "id"
    <*> o .: "name"
    <*> o .: "points"
    <*> o .: "resource_state"
    <*> o .: "starred"
    <*> o .: "start_latlng"
  parseJSON _ = empty

-- * Segment Efforts

-- | <http://strava.github.io/api/v3/efforts/#detailed>
data EffortDetailed = EffortDetailed
  { effortDetailed_activityId       :: Integer
  , effortDetailed_athleteId        :: Integer
  , effortDetailed_averageCadence   :: Maybe Double
  , effortDetailed_averageHeartrate :: Maybe Double
  , effortDetailed_averageWatts     :: Maybe Double
  , effortDetailed_distance         :: Double
  , effortDetailed_elapsedTime      :: Integer
  , effortDetailed_endIndex         :: Integer
  , effortDetailed_hidden           :: Maybe Bool
  , effortDetailed_id               :: Integer
  , effortDetailed_komRank          :: Maybe Integer
  , effortDetailed_maxHeartrate     :: Maybe Integer
  , effortDetailed_movingTime       :: Integer
  , effortDetailed_name             :: Text
  , effortDetailed_prRank           :: Maybe Integer
  , effortDetailed_resourceState    :: ResourceState
  , effortDetailed_segment          :: SegmentSummary
  , effortDetailed_startDate        :: UTCTime
  , effortDetailed_startDateLocal   :: UTCTime
  , effortDetailed_startIndex       :: Integer
  } deriving Show

instance FromJSON EffortDetailed where
  parseJSON (Object o) = EffortDetailed
    <$> ((o .: "activity") >>= (.: "id"))
    <*> ((o .: "athlete") >>= (.: "id"))
    <*> o .:? "average_cadence"
    <*> o .:? "average_heartrate"
    <*> o .:? "average_watts"
    <*> o .: "distance"
    <*> o .: "elapsed_time"
    <*> o .: "end_index"
    <*> o .:? "hidden"
    <*> o .: "id"
    <*> o .:? "kom_rank"
    <*> o .:? "max_heartrate"
    <*> o .: "moving_time"
    <*> o .: "name"
    <*> o .:? "pr_rank"
    <*> o .: "resource_state"
    <*> o .: "segment"
    <*> o .: "start_date"
    <*> o .: "start_date_local"
    <*> o .: "start_index"
  parseJSON _ = empty

-- * Streams

-- | <http://strava.github.io/api/v3/streams/#detailed>
data StreamDetailed = StreamDetailed
  { streamDetailed_data         :: [Value]
  , streamDetailed_originalSize :: Integer
  , streamDetailed_resolution   :: Resolution
  , streamDetailed_seriesType   :: SeriesType
  , streamDetailed_type         :: Text
  } deriving Show

instance FromJSON StreamDetailed where
  parseJSON (Object o) = StreamDetailed
    <$> o .: "data"
    <*> o .: "original_size"
    <*> o .: "resolution"
    <*> o .: "series_type"
    <*> o .: "type"
  parseJSON _ = empty

-- * Uploads

-- | <http://strava.github.io/api/v3/uploads/#attributes>
data UploadStatus = UploadStatus
  { uploadStatus_activityId :: Maybe Integer
  , uploadStatus_error      :: Maybe Text
  , uploadStatus_externalId :: Maybe Text
  , uploadStatus_id         :: Integer
  , uploadStatus_status     :: Text
  } deriving Show

instance FromJSON UploadStatus where
  parseJSON (Object o) = UploadStatus
    <$> o .:? "activity_id"
    <*> o .:? "error"
    <*> o .:? "external_id"
    <*> o .: "id"
    <*> o .: "status"
  parseJSON _ = empty
