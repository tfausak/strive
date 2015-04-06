-- | <http://strava.github.io/api/v3/kudos/>
module Strive.Actions.Kudos
  ( getActivityKudoers
  ) where

import Network.HTTP.Types (toQuery)
import Strive.Aliases (ActivityId, Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (GetActivityKudoersOptions)
import Strive.Types (AthleteSummary)

-- | <http://strava.github.io/api/v3/kudos/#list>
getActivityKudoers :: Client -> ActivityId -> GetActivityKudoersOptions -> IO (Result [AthleteSummary])
getActivityKudoers client activityId options = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/kudos"
  query = toQuery options
