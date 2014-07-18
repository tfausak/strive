{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Uploads
  ( UploadStatus (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/uploads/#attributes>
data UploadStatus = UploadStatus
  { uploadStatus_activityId :: Maybe Integer
  , uploadStatus_error      :: Maybe Text
  , uploadStatus_externalId :: Maybe Text
  , uploadStatus_id         :: Integer
  , uploadStatus_status     :: Text
  } deriving Show

instance FromJSON UploadStatus where
  parseJSON (Object o) = UploadStatus
    <$> o .:? "activity_id"
    <*> o .:? "error"
    <*> o .:? "external_id"
    <*> o .: "id"
    <*> o .: "status"
  parseJSON _ = empty
