-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Actions.Efforts
    ( getEffort
    ) where

import Data.Monoid ((<>))
import Strive.Client (Client)
import Strive.Client.HTTP (get)
import Strive.Objects (EffortDetailed)
import Strive.Types (EffortId)

-- | <http://strava.github.io/api/v3/efforts/#retrieve>
getEffort :: Client -> EffortId -> IO (Either String EffortDetailed)
getEffort client effortId = get client resource query
  where
    resource = "segment_efforts/" <> show effortId
    query = [] :: [(String, String)]
