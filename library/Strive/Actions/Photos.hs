-- | <http://strava.github.io/api/v3/photos/>
module Strive.Actions.Photos
  ( getActivityPhotos
  ) where

import Network.HTTP.Types (Query)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Types (PhotoSummary)

-- | <http://strava.github.io/api/v3/photos/#list>
getActivityPhotos :: Client -> Integer -> IO (Either String [PhotoSummary])
getActivityPhotos client activityId = get client resource query
  where
    resource = "api/v3/activities/" ++ show activityId ++ "/photos"
    query = [] :: Query
