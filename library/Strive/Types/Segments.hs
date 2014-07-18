{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Segments
  ( SegmentDetailed (..)
  , SegmentSummary (..)
  , SegmentLeaderboardResponse (..)
  , SegmentLeaderboardEntry (..)
  , SegmentExplorerResponse (..)
  , SegmentExplorerEntry (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (ActivityType, Gender, ResourceState)
import Strive.Types.Polylines (PolylineDetailed)

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
