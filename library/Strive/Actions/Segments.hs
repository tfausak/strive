module Strive.Actions.Segments
  ( getSegment
  , getStarredSegments
  , getSegmentEfforts
  , getSegmentLeaderboard
  , exploreSegments
  ) where

import Data.List (intercalate)
import Network.HTTP.Types (Query, toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (ExploreSegmentsOptions, GetSegmentEffortsOptions,
                       GetSegmentLeaderboardOptions, GetStarredSegmentsOptions)
import Strive.Types (EffortDetailed, SegmentDetailed, SegmentExplorerResponse,
                     SegmentLeaderboardResponse, SegmentSummary)

-- | <http://strava.github.io/api/v3/segments/#retrieve>
getSegment :: Client -> Integer -> IO (Either String SegmentDetailed)
getSegment client segmentId = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/segments/#starred>
getStarredSegments :: Client -> GetStarredSegmentsOptions -> IO (Either String [SegmentSummary])
getStarredSegments client options = get client resource query
 where
  resource = "api/v3/segments/starred"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#efforts>
getSegmentEfforts :: Client -> Integer -> GetSegmentEffortsOptions -> IO (Either String [EffortDetailed])
getSegmentEfforts client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId ++ "/all_efforts"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
getSegmentLeaderboard :: Client -> Integer -> GetSegmentLeaderboardOptions -> IO (Either String SegmentLeaderboardResponse)
getSegmentLeaderboard client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId ++ "/leaderboard"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#explore>
exploreSegments :: Client -> (Double, Double, Double, Double) -> ExploreSegmentsOptions -> IO (Either String SegmentExplorerResponse)
exploreSegments client (south, west, north, east) options = get client resource query
 where
  resource = "api/v3/segments/explore"
  query = toQuery
    [ ("bounds", intercalate "," (map show [south, west, north, east]))
    ] ++ toQuery options
