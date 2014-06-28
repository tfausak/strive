{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#detailed>
module Strive.Objects.Activities.ActivityDetailed
    ( ActivityDetailed (..)
    ) where

import           Control.Applicative      (empty, (<$>), (<*>))
import           Data.Aeson               (FromJSON, Value (Object), parseJSON,
                                           (.:), (.:?))
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           Strive.Objects.Athletes  (AthleteMeta)
import           Strive.Objects.Efforts   (EffortSummary)
import           Strive.Objects.Gear      (GearSummary)
import           Strive.Objects.Polylines (PolylineDetailed)

-- | Detailed representation of an activity.
data ActivityDetailed = ActivityDetailed
    { achievementCount      :: Integer
    , athlete               :: AthleteMeta
    , athleteCount          :: Integer
    , averageSpeed          :: Double
    , averageWatts          :: Maybe Double
    , calories              :: Double
    , commentCount          :: Integer
    , commute               :: Bool
    , description           :: Text
    , distance              :: Double
    , elapsedTime           :: Integer
    , endLatlng             :: Maybe (Double, Double)
    , externalId            :: Maybe Text
    , flagged               :: Bool
    , gear                  :: GearSummary
    , gearId                :: Maybe Text
    , hasKudoed             :: Bool
    , id                    :: Integer
    , instagramPrimaryPhoto :: Text
    , kilojoules            :: Maybe Double
    , locationCity          :: Maybe Text
    , locationCountry       :: Text
    , locationState         :: Maybe Text
    , manual                :: Bool
    , map                   :: PolylineDetailed
    , maxSpeed              :: Double
    , movingTime            :: Integer
    , name                  :: Text
    , photoCount            :: Integer
    , private               :: Bool
    , resourceState         :: Integer
    , segmentEfforts        :: [EffortSummary]
    , startDate             :: UTCTime
    , startDateLocal        :: UTCTime
    , startLatitude         :: Double
    , startLatlng           :: Maybe (Double, Double)
    , startLongitude        :: Double
    , timezone              :: Text
    , totalElevationGain    :: Double
    , trainer               :: Bool
    , truncated             :: Integer
    , type_                 :: Text
    , uploadId              :: Maybe Integer
    } deriving Show

instance FromJSON ActivityDetailed where
    parseJSON (Object o) = ActivityDetailed
        <$> o .: "achievement_count"
        <*> o .: "athlete"
        <*> o .: "athlete_count"
        <*> o .: "average_speed"
        <*> o .:? "average_watts"
        <*> o .: "calories"
        <*> o .: "comment_count"
        <*> o .: "commute"
        <*> o .: "description"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .:? "end_latlng"
        <*> o .:? "external_id"
        <*> o .: "flagged"
        <*> o .: "gear"
        <*> o .:? "gear_id"
        <*> o .: "has_kudoed"
        <*> o .: "id"
        <*> o .: "instagram_primary_photo"
        <*> o .:? "kilojoules"
        <*> o .:? "location_city"
        <*> o .: "location_country"
        <*> o .:? "location_state"
        <*> o .: "manual"
        <*> o .: "map"
        <*> o .: "max_speed"
        <*> o .: "moving_time"
        <*> o .: "name"
        <*> o .: "photo_count"
        <*> o .: "private"
        <*> o .: "resource_state"
        <*> o .: "segment_efforts"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
        <*> o .: "start_latitude"
        <*> o .:? "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "timezone"
        <*> o .: "total_elevation_gain"
        <*> o .: "trainer"
        <*> o .: "truncated"
        <*> o .: "type"
        <*> o .:? "upload_id"
    parseJSON _ = empty
