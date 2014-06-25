{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#detailed>
module Strive.Objects.Polylines.PolylineSummary
    ( PolylineSummary (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:), (.:?))
import           Data.Text           (Text)

-- | Detailed representation of a polyline.
data PolylineSummary = PolylineSummary
    { id              :: Text
    , resourceState   :: Integer
    , summaryPolyline :: Maybe Text
    } deriving Show

instance FromJSON PolylineSummary where
    parseJSON (Object o) = PolylineSummary
        <$> o .: "id"
        <*> o .: "resource_state"
        <*> o .:? "summary_polyline"
    parseJSON _ = empty
