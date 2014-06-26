-- | <http://strava.github.io/api/v3/kudos/>
module Strive.Actions.Kudos
    ( getActivityKudoers
    ) where

import           Data.Monoid      ((<>))
import           Strive.Client    (Client)
import           Strive.Objects   (AthleteSummary)
import           Strive.Types     (ActivityId, Page, PerPage)
import           Strive.Utilities (get, paginate)

-- | <http://strava.github.io/api/v3/kudos/#list>
getActivityKudoers :: Client -> ActivityId -> Page -> PerPage -> IO (Either String [AthleteSummary])
getActivityKudoers client activityId page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/kudos"
    query = paginate page perPage
