module Strive.Actions.Efforts
  ( getSegmentEffort
  ) where

-- | <http://strava.github.io/api/v3/efforts/#retrieve>
getSegmentEffort :: Client -> Integer -> IO (Either String T.EffortDetailed)
getSegmentEffort client effortId = get client resource query
 where
  resource = "api/v3/segment_efforts/" <> show effortId
  query = [] :: Query
