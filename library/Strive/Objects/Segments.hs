{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/>
module Strive.Objects.Segments
    ( SegmentDetailed (..)
    , SegmentSummary (..)
    , SegmentLeaderboard (..)
    , SegmentLeaderboardEntry (..)
    , SegmentExplorer (..)
    , SegmentExplorerEntry (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Polylines (PolylineDetailed)

-- | <http://strava.github.io/api/v3/segments/#detailed>
data SegmentDetailed = SegmentDetailed
    { _segmentDetailed_activityType       :: Text
    , _segmentDetailed_athleteCount       :: Integer
    , _segmentDetailed_averageGrade       :: Double
    , _segmentDetailed_city               :: Text
    , _segmentDetailed_climbCategory      :: Integer
    , _segmentDetailed_country            :: Text
    , _segmentDetailed_createdAt          :: UTCTime
    , _segmentDetailed_distance           :: Double
    , _segmentDetailed_effortCount        :: Integer
    , _segmentDetailed_elevationHigh      :: Double
    , _segmentDetailed_elevationLow       :: Double
    , _segmentDetailed_endLatitude        :: Double
    , _segmentDetailed_endLatlng          :: (Double, Double)
    , _segmentDetailed_endLongitude       :: Double
    , _segmentDetailed_hazardous          :: Bool
    , _segmentDetailed_id                 :: Integer
    , _segmentDetailed_map                :: PolylineDetailed
    , _segmentDetailed_maximumGrade       :: Double
    , _segmentDetailed_name               :: Text
    , _segmentDetailed_private            :: Bool
    , _segmentDetailed_resourceState      :: Integer
    , _segmentDetailed_starCount          :: Integer
    , _segmentDetailed_starred            :: Bool
    , _segmentDetailed_startLatitude      :: Double
    , _segmentDetailed_startLatlng        :: (Double, Double)
    , _segmentDetailed_startLongitude     :: Double
    , _segmentDetailed_state              :: Text
    , _segmentDetailed_totalElevationGain :: Double
    , _segmentDetailed_updatedAt          :: UTCTime
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
    { _segmentSummary_activityType   :: Text
    , _segmentSummary_averageGrade   :: Double
    , _segmentSummary_city           :: Text
    , _segmentSummary_climbCategory  :: Integer
    , _segmentSummary_country        :: Text
    , _segmentSummary_distance       :: Double
    , _segmentSummary_elevationHigh  :: Double
    , _segmentSummary_elevationLow   :: Double
    , _segmentSummary_endLatitude    :: Double
    , _segmentSummary_endLatlng      :: (Double, Double)
    , _segmentSummary_endLongitude   :: Double
    , _segmentSummary_id             :: Integer
    , _segmentSummary_maximumGrade   :: Double
    , _segmentSummary_name           :: Text
    , _segmentSummary_private        :: Bool
    , _segmentSummary_resourceState  :: Integer
    , _segmentSummary_starred        :: Bool
    , _segmentSummary_startLatitude  :: Double
    , _segmentSummary_startLatlng    :: (Double, Double)
    , _segmentSummary_startLongitude :: Double
    , _segmentSummary_state          :: Text
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
data SegmentLeaderboard = SegmentLeaderboard
    { _segmentLeaderboard_entries :: [SegmentLeaderboardEntry]
    } deriving Show

instance FromJSON SegmentLeaderboard where
    parseJSON (Object o) = SegmentLeaderboard
        <$> o .: "segments"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
data SegmentLeaderboardEntry = SegmentLeaderboardEntry
    { _segmentLeaderboardEntry_activityId     :: Integer
    , _segmentLeaderboardEntry_athleteGender  :: Maybe Char
    , _segmentLeaderboardEntry_athleteId      :: Integer
    , _segmentLeaderboardEntry_athleteName    :: Text
    , _segmentLeaderboardEntry_athleteProfile :: Text
    , _segmentLeaderboardEntry_averageHr      :: Double
    , _segmentLeaderboardEntry_averageWatts   :: Double
    , _segmentLeaderboardEntry_distance       :: Double
    , _segmentLeaderboardEntry_effortId       :: Integer
    , _segmentLeaderboardEntry_elapsedTime    :: Integer
    , _segmentLeaderboardEntry_movingTime     :: Integer
    , _segmentLeaderboardEntry_rank           :: Integer
    , _segmentLeaderboardEntry_startDate      :: UTCTime
    , _segmentLeaderboardEntry_startDateLocal :: UTCTime
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
data SegmentExplorer = SegmentExplorer
    { _segmentExplorer_entries :: [SegmentExplorerEntry]
    } deriving Show

instance FromJSON SegmentExplorer where
    parseJSON (Object o) = SegmentExplorer
        <$> o .: "entries"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/segments/#explore>
data SegmentExplorerEntry = SegmentExplorerEntry
    { _segmentExplorerEntry_avgGrade          :: Double
    , _segmentExplorerEntry_climbCategory     :: Integer
    , _segmentExplorerEntry_climbCategoryDesc :: String
    , _segmentExplorerEntry_distance          :: Double
    , _segmentExplorerEntry_elevDifference    :: Double
    , _segmentExplorerEntry_endLatlng         :: (Double, Double)
    , _segmentExplorerEntry_id                :: Integer
    , _segmentExplorerEntry_name              :: Text
    , _segmentExplorerEntry_points            :: Text
    , _segmentExplorerEntry_resourceState     :: Integer
    , _segmentExplorerEntry_starred           :: Bool
    , _segmentExplorerEntry_startLatlng       :: (Double, Double)
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
