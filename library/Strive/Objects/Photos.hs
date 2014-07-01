{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/photos/>
module Strive.Objects.Photos
    ( PhotoSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | <http://strava.github.io/api/v3/photos/#summary-and-detailed-representation-attributes>
data PhotoSummary = PhotoSummary
    { _photoSummary_activityId    :: Integer
    , _photoSummary_caption       :: Text
    , _photoSummary_createdAt     :: UTCTime
    , _photoSummary_id            :: Integer
    , _photoSummary_location      :: Maybe (Double, Double)
    , _photoSummary_ref           :: Text
    , _photoSummary_resourceState :: Integer
    , _photoSummary_type          :: Text
    , _photoSummary_uid           :: Text
    , _photoSummary_uploadedAt    :: UTCTime
    } deriving Show

instance FromJSON PhotoSummary where
    parseJSON (Object o) = PhotoSummary
        <$> o .: "activity_id"
        <*> o .: "caption"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .:? "location"
        <*> o .: "ref"
        <*> o .: "resource_state"
        <*> o .: "type"
        <*> o .: "uid"
        <*> o .: "uploaded_at"
    parseJSON _ = empty
