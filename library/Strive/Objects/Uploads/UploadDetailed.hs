{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/uploads/#get-status>
module Strive.Objects.Uploads.UploadDetailed
    ( UploadDetailed (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:), (.:?))
import           Data.Text           (Text)

-- | Detailed representation of an upload.
data UploadDetailed = UploadDetailed
    { activityId :: Maybe Integer
    , error      :: Maybe Text
    , externalId :: Text
    , id         :: Integer
    , status     :: Text
    } deriving Show

instance FromJSON UploadDetailed where
    parseJSON (Object o) = UploadDetailed
        <$> o .:? "activity_id"
        <*> o .:? "error"
        <*> o .: "external_id"
        <*> o .: "id"
        <*> o .: "status"
    parseJSON _ = empty
