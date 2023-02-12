-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Actions.Efforts
  ( getSegmentEffort,
  )
where

import Network.HTTP.Types (Query)
import Strive.Aliases (EffortId, Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Types (EffortDetailed)

-- | <http://strava.github.io/api/v3/efforts/#retrieve>
getSegmentEffort :: Client -> EffortId -> IO (Result EffortDetailed)
getSegmentEffort client effortId = get client resource query
  where
    resource = "api/v3/segment_efforts/" <> show effortId
    query = [] :: Query
