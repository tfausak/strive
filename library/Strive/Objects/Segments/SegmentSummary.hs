{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#summary>
module Strive.Objects.Segments.SegmentSummary
    ( SegmentSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)

-- | Summary representation of an effort.
data SegmentSummary = SegmentSummary
    { activityType   :: Text
    , averageGrade   :: Double
    , city           :: Text
    , climbCategory  :: Integer
    , country        :: Text
    , distance       :: Double
    , elevationHigh  :: Double
    , elevationLow   :: Double
    , endLatitude    :: Double
    , endLatlng      :: (Double, Double)
    , endLongitude   :: Double
    , id             :: Integer
    , maximumGrade   :: Double
    , name           :: Text
    , private        :: Bool
    , resourceState  :: Integer
    , starred        :: Bool
    , startLatitude  :: Double
    , startLatlng    :: (Double, Double)
    , startLongitude :: Double
    , state          :: Text
    } deriving Show

instance FromJSON SegmentSummary where
    parseJSON (Object o) = SegmentSummary
        <$> o .: "activity_type"
        <*> o .: "average_grade"
        <*> o .: "city"
        <*> o .: "climb_category"
        <*> o .: "country"
        <*> o .: "distance"
        <*> o .: "elevation_high"
        <*> o .: "elevation_low"
        <*> o .: "end_latitude"
        <*> o .: "end_latlng"
        <*> o .: "end_longitude"
        <*> o .: "id"
        <*> o .: "maximum_grade"
        <*> o .: "name"
        <*> o .: "private"
        <*> o .: "resource_state"
        <*> o .: "starred"
        <*> o .: "start_latitude"
        <*> o .: "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "state"
    parseJSON _ = empty
