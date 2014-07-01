{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/uploads/>
module Strive.Objects.Uploads
    ( UploadDetailed (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/uploads/#attributes>
data UploadDetailed = UploadDetailed
    { uploadDetailedActivityId :: Maybe Integer
    , uploadDetailedError      :: Maybe Text
    , uploadDetailedExternalId :: Text
    , uploadDetailedId         :: Integer
    , uploadDetailedStatus     :: Text
    } deriving Show

instance FromJSON UploadDetailed where
    parseJSON (Object o) = UploadDetailed
        <$> o .:? "activity_id"
        <*> o .:? "error"
        <*> o .: "external_id"
        <*> o .: "id"
        <*> o .: "status"
    parseJSON _ = empty
