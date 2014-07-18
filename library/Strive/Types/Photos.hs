{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Photos
  ( PhotoSummary (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (PhotoType, ResourceState)

-- | <http://strava.github.io/api/v3/photos/#summary-and-detailed-representation-attributes>
data PhotoSummary = PhotoSummary
  { photoSummary_activityId    :: Integer
  , photoSummary_caption       :: Text
  , photoSummary_createdAt     :: UTCTime
  , photoSummary_id            :: Integer
  , photoSummary_location      :: Maybe (Double, Double)
  , photoSummary_ref           :: Text
  , photoSummary_resourceState :: ResourceState
  , photoSummary_type          :: PhotoType
  , photoSummary_uid           :: Text
  , photoSummary_uploadedAt    :: UTCTime
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
