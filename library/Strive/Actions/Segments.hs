-- | <http://strava.github.io/api/v3/segments/>
module Strive.Actions.Segments
  ( getSegment
  , getStarredSegments
  , getSegmentEfforts
  , getSegmentLeaderboard
  , exploreSegments
  ) where

import Data.List (intercalate)
import Network.HTTP.Types (Query, toQuery)
import Strive.Aliases (Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (ExploreSegmentsOptions, GetSegmentEffortsOptions,
                       GetSegmentLeaderboardOptions, GetStarredSegmentsOptions)
import Strive.Types (EffortDetailed, SegmentDetailed, SegmentExplorerResponse,
                     SegmentLeaderboardResponse, SegmentSummary)

-- | <http://strava.github.io/api/v3/segments/#retrieve>
getSegment :: Client -> Integer -> Result SegmentDetailed
getSegment client segmentId = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/segments/#starred>
getStarredSegments :: Client -> GetStarredSegmentsOptions -> Result [SegmentSummary]
getStarredSegments client options = get client resource query
 where
  resource = "api/v3/segments/starred"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#efforts>
getSegmentEfforts :: Client -> Integer -> GetSegmentEffortsOptions -> Result [EffortDetailed]
getSegmentEfforts client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId ++ "/all_efforts"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#leaderboard>
getSegmentLeaderboard :: Client -> Integer -> GetSegmentLeaderboardOptions -> Result SegmentLeaderboardResponse
getSegmentLeaderboard client segmentId options = get client resource query
 where
  resource = "api/v3/segments/" ++ show segmentId ++ "/leaderboard"
  query = toQuery options

-- | <http://strava.github.io/api/v3/segments/#explore>
exploreSegments :: Client -> (Double, Double, Double, Double) -> ExploreSegmentsOptions -> Result SegmentExplorerResponse
exploreSegments client (south, west, north, east) options = get client resource query
 where
  resource = "api/v3/segments/explore"
  query = toQuery
    [ ("bounds", intercalate "," (map show [south, west, north, east]))
    ] ++ toQuery options
