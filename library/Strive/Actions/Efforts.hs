-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Actions.Efforts
    ( getEffort
    ) where

import           Data.Monoid             ((<>))
import           Strive.Actions.Internal (get)
import           Strive.Client           (Client)
import           Strive.Objects          (EffortSummary)
import           Strive.Types            (EffortId)

-- | <http://strava.github.io/api/v3/efforts/#retrieve>
getEffort :: Client -> EffortId -> IO (Either String EffortSummary)
getEffort client effortId = get client resource query
  where
    resource = "segment_efforts/" <> show effortId
    query = []
