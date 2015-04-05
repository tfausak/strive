-- | <http://strava.github.io/api/v3/comments/>
module Strive.Actions.Comments
  ( getActivityComments
  ) where

import Network.HTTP.Types (toQuery)
import Strive.Aliases (Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Options (GetActivityCommentsOptions)
import Strive.Types (CommentSummary)

-- TODO: Move to Strive.Aliases.
type ActivityId = Integer

-- | <http://strava.github.io/api/v3/comments/#list>
getActivityComments :: Client -> ActivityId -> GetActivityCommentsOptions -> Result [CommentSummary]
getActivityComments client activityId options = get client resource query
 where
  resource = "api/v3/activities/" ++ show activityId ++ "/comments"
  query = toQuery options
