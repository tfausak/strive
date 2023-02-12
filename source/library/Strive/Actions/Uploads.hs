-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Actions.Uploads
  ( uploadActivity,
    getUpload,
  )
where

import Data.ByteString (ByteString)
import Network.HTTP.Client (RequestBody (RequestBodyBS), requestBody)
import Network.HTTP.Types (Query, methodPost, toQuery)
import Strive.Aliases (Extension, Result, UploadId)
import Strive.Client (Client)
import Strive.Internal.HTTP (buildRequest, get, handleResponse, performRequest)
import Strive.Options (UploadActivityOptions)
import Strive.Types (UploadStatus)

-- | <http://strava.github.io/api/v3/uploads/#post-file>
uploadActivity ::
  Client ->
  ByteString ->
  Extension ->
  UploadActivityOptions ->
  IO (Result UploadStatus)
uploadActivity client body dataType options = do
  initialRequest <- buildRequest methodPost client resource query
  let request = initialRequest {requestBody = RequestBodyBS body}
  response <- performRequest client request
  return (handleResponse response)
  where
    resource = "api/v3/uploads"
    query = toQuery [("data_type", dataType)] <> toQuery options

-- | <http://strava.github.io/api/v3/uploads/#get-status>
getUpload :: Client -> UploadId -> IO (Result UploadStatus)
getUpload client uploadId = get client resource query
  where
    resource = "api/v3/uploads/" <> show uploadId
    query = [] :: Query
