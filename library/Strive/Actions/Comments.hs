module Strive.Actions.Comments
  ( getActivityComments
  ) where

-- | <http://strava.github.io/api/v3/comments/#list>
getActivityComments :: Client -> Integer -> O.GetActivityCommentsOptions -> IO (Either String [T.CommentSummary])
getActivityComments client activityId options = get client resource query
 where
  resource = "api/v3/activities/" <> show activityId <> "/comments"
  query = toQuery options
