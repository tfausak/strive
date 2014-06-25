{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#detailed>
module Strive.Objects.Segments.SegmentDetailed
    ( SegmentDetailed (..)
    ) where

import           Control.Applicative      (empty, (<$>), (<*>))
import           Data.Aeson               (FromJSON, Value (Object), parseJSON,
                                           (.:))
import           Data.Text                (Text)
import           Data.Time.Clock          (UTCTime)
import           Strive.Objects.Polylines (PolylineDetailed)

-- | Detailed representation of an effort.
data SegmentDetailed = SegmentDetailed
    { activityType       :: Text
    , athleteCount       :: Integer
    , averageGrade       :: Double
    , city               :: Text
    , climbCategory      :: Integer
    , country            :: Text
    , createdAt          :: UTCTime
    , distance           :: Double
    , effortCount        :: Integer
    , elevationHigh      :: Double
    , elevationLow       :: Double
    , endLatitude        :: Double
    , endLatlng          :: (Double, Double)
    , endLongitude       :: Double
    , hazardous          :: Bool
    , id                 :: Integer
    , map                :: PolylineDetailed
    , maximumGrade       :: Double
    , name               :: Text
    , private            :: Bool
    , resourceState      :: Integer
    , starCount          :: Integer
    , starred            :: Bool
    , startLatitude      :: Double
    , startLatlng        :: (Double, Double)
    , startLongitude     :: Double
    , state              :: Text
    , totalElevationGain :: Double
    , updatedAt          :: UTCTime
    } deriving Show

instance FromJSON SegmentDetailed where
    parseJSON (Object o) = SegmentDetailed
        <$> o .: "activity_type"
        <*> o .: "athlete_count"
        <*> o .: "average_grade"
        <*> o .: "city"
        <*> o .: "climb_category"
        <*> o .: "country"
        <*> o .: "created_at"
        <*> o .: "distance"
        <*> o .: "effort_count"
        <*> o .: "elevation_high"
        <*> o .: "elevation_low"
        <*> o .: "end_latitude"
        <*> o .: "end_latlng"
        <*> o .: "end_longitude"
        <*> o .: "hazardous"
        <*> o .: "id"
        <*> o .: "map"
        <*> o .: "maximum_grade"
        <*> o .: "name"
        <*> o .: "private"
        <*> o .: "resource_state"
        <*> o .: "star_count"
        <*> o .: "starred"
        <*> o .: "start_latitude"
        <*> o .: "start_latlng"
        <*> o .: "start_longitude"
        <*> o .: "state"
        <*> o .: "total_elevation_gain"
        <*> o .: "updated_at"
    parseJSON _ = empty
