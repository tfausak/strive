{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Objects.Uploads
    ( UploadStatus (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/uploads/#attributes>
data UploadStatus = UploadStatus
    { _uploadStatus_activityId :: Maybe Integer
    , _uploadStatus_error      :: Maybe Text
    , _uploadStatus_externalId :: Text
    , _uploadStatus_id         :: Integer
    , _uploadStatus_status     :: Text
    } deriving Show

instance FromJSON UploadStatus where
    parseJSON (Object o) = UploadStatus
        <$> o .:? "activity_id"
        <*> o .:? "error"
        <*> o .: "external_id"
        <*> o .: "id"
        <*> o .: "status"
    parseJSON _ = empty
