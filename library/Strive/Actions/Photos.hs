-- | <http://strava.github.io/api/v3/photos/>
module Strive.Actions.Photos
    ( getActivityPhotos
    ) where

import           Data.Monoid             ((<>))
import           Strive.Actions.Internal (get)
import           Strive.Client           (Client)
import           Strive.Objects          (PhotoSummary)
import           Strive.Types            (ActivityId)

-- | <http://strava.github.io/api/v3/photos/#list>
getActivityPhotos :: Client -> ActivityId -> IO (Either String [PhotoSummary])
getActivityPhotos client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/photos"
    query = []
