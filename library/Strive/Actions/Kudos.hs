module Strive.Actions.Kudos
  ( getActivityKudoers
  ) where

-- | <http://strava.github.io/api/v3/kudos/#list>
getActivityKudoers :: Client -> Integer -> O.GetActivityKudoersOptions -> IO (Either String [T.AthleteSummary])
getActivityKudoers client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/kudos"
  query = toQuery options
