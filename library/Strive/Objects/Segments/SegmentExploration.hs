{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#explore>
module Strive.Objects.Segments.SegmentExploration
    ( SegmentExploration (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)

-- | Representation of a segment while exploring.
data SegmentExploration = SegmentExploration
    { avgGrade          :: Double
    , climbCategory     :: Integer
    , climbCategoryDesc :: String
    , distance          :: Double
    , elevDifference    :: Double
    , endLatlng         :: (Double, Double)
    , id                :: Integer
    , name              :: Text
    , points            :: Text
    , resourceState     :: Integer
    , starred           :: Bool
    , startLatlng       :: (Double, Double)
    } deriving Show

instance FromJSON SegmentExploration where
    parseJSON (Object o) = SegmentExploration
        <$> o .: "avg_grade"
        <*> o .: "climb_category"
        <*> o .: "climb_category_desc"
        <*> o .: "distance"
        <*> o .: "elev_difference"
        <*> o .: "end_latlng"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "points"
        <*> o .: "resource_state"
        <*> o .: "starred"
        <*> o .: "start_latlng"
    parseJSON _ = empty
