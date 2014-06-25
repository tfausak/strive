{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#detailed>
module Strive.Objects.Polylines.PolylineDetailed
    ( PolylineDetailed (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:), (.:?))
import           Data.Text           (Text)

-- | Detailed representation of a polyline.
data PolylineDetailed = PolylineDetailed
    { id              :: Text
    , polyline        :: Text
    , resourceState   :: Integer
    , summaryPolyline :: Maybe Text
    } deriving Show

instance FromJSON PolylineDetailed where
    parseJSON (Object o) = PolylineDetailed
        <$> o .: "id"
        <*> o .: "polyline"
        <*> o .: "resource_state"
        <*> o .:? "summary_polyline"
    parseJSON _ = empty
