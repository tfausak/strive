{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/segments/>
module Strive.Types.Segments
  ( SegmentDetailed (..)
  , SegmentSummary (..)
  , SegmentLeaderboardResponse (..)
  , SegmentLeaderboardEntry (..)
  , SegmentExplorerResponse (..)
  , SegmentExplorerEntry (..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (ActivityType, Gender, ResourceState)
import Strive.Internal.TH (options)
import Strive.Types.Polylines (Polyline, PolylineDetailed)

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

$(deriveFromJSON options ''SegmentDetailed)

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

$(deriveFromJSON options ''SegmentSummary)

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

$(deriveFromJSON options ''SegmentLeaderboardEntry)

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
data SegmentLeaderboardResponse = SegmentLeaderboardResponse
  { segmentLeaderboardResponse_effortCount :: Integer
  , segmentLeaderboardResponse_entryCount  :: Integer
  , segmentLeaderboardResponse_entries     :: [SegmentLeaderboardEntry]
  } deriving Show

$(deriveFromJSON options ''SegmentLeaderboardResponse)

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
  , segmentExplorerEntry_points            :: Polyline
  , segmentExplorerEntry_resourceState     :: ResourceState
  , segmentExplorerEntry_starred           :: Bool
  , segmentExplorerEntry_startLatlng       :: (Double, Double)
  } deriving Show

$(deriveFromJSON options ''SegmentExplorerEntry)

-- | <http://strava.github.io/api/v3/segments/#explore>
data SegmentExplorerResponse = SegmentExplorerResponse
  { segmentExplorerResponse_segments :: [SegmentExplorerEntry]
  } deriving Show

$(deriveFromJSON options ''SegmentExplorerResponse)
