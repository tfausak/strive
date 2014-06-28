-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Actions.Uploads
    ( getUpload
    ) where

import           Data.Monoid      ((<>))
import           Strive.Client    (Client)
import           Strive.Objects   (UploadDetailed)
import           Strive.Utilities (get)

-- | <http://strava.github.io/api/v3/uploads/#get-status>
getUpload :: Client -> Integer -> IO (Either String UploadDetailed)
getUpload client uploadId = get client resource query
  where
    resource = "uploads/" <> show uploadId
    query = []
