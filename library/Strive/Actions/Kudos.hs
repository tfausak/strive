module Strive.Actions.Kudos
  ( getActivityKudoers
  ) where

import Network.HTTP.Types (toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (GetActivityKudoersOptions)
import Strive.Types (AthleteSummary)

-- | <http://strava.github.io/api/v3/kudos/#list>
getActivityKudoers :: Client -> Integer -> GetActivityKudoersOptions -> IO (Either String [AthleteSummary])
getActivityKudoers client activityId options = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/kudos"
  query = toQuery options
