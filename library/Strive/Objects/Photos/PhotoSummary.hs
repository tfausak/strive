{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/photos/#summary-and-detailed-representation-attributes>
module Strive.Objects.Photos.PhotoSummary
    ( PhotoSummary (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:), (.:?))
import           Data.Text           (Text)
import           Data.Time.Clock     (UTCTime)

-- | Summary representation of a photo.
data PhotoSummary = PhotoSummary
    { activityId    :: Integer
    , caption       :: Text
    , createdAt     :: UTCTime
    , id            :: Integer
    , location      :: Maybe (Double, Double)
    , ref           :: Text
    , resourceState :: Integer
    , type_         :: Text
    , uid           :: Text
    , uploadedAt    :: UTCTime
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
