{-# LANGUAGE OverloadedStrings #-}

-- | Data types representing responses from the API.
module Strive.Types where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Lenses
import Strive.Lenses.Classes

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
  , athleteDetailed_measurementPreference :: Text
  , athleteDetailed_mutualFriendCount     :: Integer
  , athleteDetailed_premium               :: Bool
  , athleteDetailed_profile               :: Text
  , athleteDetailed_profileMedium         :: Text
  , athleteDetailed_resourceState         :: Integer
  , athleteDetailed_sex                   :: Maybe Char
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
  , athleteSummary_resourceState :: Integer
  , athleteSummary_sex           :: Maybe Char
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

-- * Clubs

-- | <http://strava.github.io/api/v3/clubs/#summary>
data ClubSummary = ClubSummary
  { clubSummary_id            :: Integer
  , clubSummary_name          :: Text
  , clubSummary_profile       :: Text
  , clubSummary_profileMedium :: Text
  , clubSummary_resourceState :: Integer
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

-- | <http://strava.github.io/api/v3/gear/#summary>
data GearSummary = GearSummary
  { gearSummary_distance      :: Double
  , gearSummary_id            :: Text
  , gearSummary_name          :: Text
  , gearSummary_primary       :: Bool
  , gearSummary_resourceState :: Integer
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

-- | <http://strava.github.io/api/v3/segments/#summary>
data SegmentSummary = SegmentSummary
  { segmentSummary_activityType   :: Text
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
  , segmentSummary_resourceState  :: Integer
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
  , effortDetailed_resourceState    :: Integer
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
