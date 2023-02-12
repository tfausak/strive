{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Types.Uploads
  ( UploadStatus (..),
  )
where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Internal.TH (options)

-- | <http://strava.github.io/api/v3/uploads/#attributes>
data UploadStatus = UploadStatus
  { uploadStatus_activityId :: Maybe Integer,
    uploadStatus_error :: Maybe Text,
    uploadStatus_externalId :: Maybe Text,
    uploadStatus_id :: Integer,
    uploadStatus_status :: Text
  }
  deriving (Show)

$(deriveFromJSON options ''UploadStatus)
