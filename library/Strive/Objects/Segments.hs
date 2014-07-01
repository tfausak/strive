{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/>
module Strive.Objects.Segments
    ( SegmentDetailed (..)
    , SegmentSummary (..)
    , SegmentLeaderboardEntry (..)
    , SegmentExplorerEntry (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Polylines (PolylineDetailed)

-- | <http://strava.github.io/api/v3/segments/#detailed>
data SegmentDetailed = SegmentDetailed
    { segmentDetailedActivityType       :: Text
    , segmentDetailedAthleteCount       :: Integer
    , segmentDetailedAverageGrade       :: Double
    , segmentDetailedCity               :: Text
    , segmentDetailedClimbCategory      :: Integer
    , segmentDetailedCountry            :: Text
    , segmentDetailedCreatedAt          :: UTCTime
    , segmentDetailedDistance           :: Double
    , segmentDetailedEffortCount        :: Integer
    , segmentDetailedElevationHigh      :: Double
    , segmentDetailedElevationLow       :: Double
    , segmentDetailedEndLatitude        :: Double
    , segmentDetailedEndLatlng          :: (Double, Double)
    , segmentDetailedEndLongitude       :: Double
    , segmentDetailedHazardous          :: Bool
    , segmentDetailedId                 :: Integer
    , segmentDetailedMap                :: PolylineDetailed
    , segmentDetailedMaximumGrade       :: Double
    , segmentDetailedName               :: Text
    , segmentDetailedPrivate            :: Bool
    , segmentDetailedResourceState      :: Integer
    , segmentDetailedStarCount          :: Integer
    , segmentDetailedStarred            :: Bool
    , segmentDetailedStartLatitude      :: Double
    , segmentDetailedStartLatlng        :: (Double, Double)
    , segmentDetailedStartLongitude     :: Double
    , segmentDetailedState              :: Text
    , segmentDetailedTotalElevationGain :: Double
    , segmentDetailedUpdatedAt          :: UTCTime
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
    { segmentSummaryActivityType   :: Text
    , segmentSummaryAverageGrade   :: Double
    , segmentSummaryCity           :: Text
    , segmentSummaryClimbCategory  :: Integer
    , segmentSummaryCountry        :: Text
    , segmentSummaryDistance       :: Double
    , segmentSummaryElevationHigh  :: Double
    , segmentSummaryElevationLow   :: Double
    , segmentSummaryEndLatitude    :: Double
    , segmentSummaryEndLatlng      :: (Double, Double)
    , segmentSummaryEndLongitude   :: Double
    , segmentSummaryId             :: Integer
    , segmentSummaryMaximumGrade   :: Double
    , segmentSummaryName           :: Text
    , segmentSummaryPrivate        :: Bool
    , segmentSummaryResourceState  :: Integer
    , segmentSummaryStarred        :: Bool
    , segmentSummaryStartLatitude  :: Double
    , segmentSummaryStartLatlng    :: (Double, Double)
    , segmentSummaryStartLongitude :: Double
    , segmentSummaryState          :: Text
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
data SegmentLeaderboardEntry = SegmentLeaderboardEntry
    { segmentLeaderboardEntryActivityId     :: Integer
    , segmentLeaderboardEntryAthleteGender  :: Maybe Char
    , segmentLeaderboardEntryAthleteId      :: Integer
    , segmentLeaderboardEntryAthleteName    :: Text
    , segmentLeaderboardEntryAthleteProfile :: Text
    , segmentLeaderboardEntryAverageHr      :: Double
    , segmentLeaderboardEntryAverageWatts   :: Double
    , segmentLeaderboardEntryDistance       :: Double
    , segmentLeaderboardEntryEffortId       :: Integer
    , segmentLeaderboardEntryElapsedTime    :: Integer
    , segmentLeaderboardEntryMovingTime     :: Integer
    , segmentLeaderboardEntryRank           :: Integer
    , segmentLeaderboardEntryStartDate      :: UTCTime
    , segmentLeaderboardEntryStartDateLocal :: UTCTime
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
data SegmentExplorerEntry = SegmentExplorerEntry
    { segmentExplorerEntryAvgGrade          :: Double
    , segmentExplorerEntryClimbCategory     :: Integer
    , segmentExplorerEntryClimbCategoryDesc :: String
    , segmentExplorerEntryDistance          :: Double
    , segmentExplorerEntryElevDifference    :: Double
    , segmentExplorerEntryEndLatlng         :: (Double, Double)
    , segmentExplorerEntryId                :: Integer
    , segmentExplorerEntryName              :: Text
    , segmentExplorerEntryPoints            :: Text
    , segmentExplorerEntryResourceState     :: Integer
    , segmentExplorerEntryStarred           :: Bool
    , segmentExplorerEntryStartLatlng       :: (Double, Double)
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
