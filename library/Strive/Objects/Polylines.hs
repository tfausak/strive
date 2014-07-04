{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/#polylines>
module Strive.Objects.Polylines
    ( PolylineDetailed (..)
    , PolylineSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import GPolyline (decodeline)

-- | <http://strava.github.io/api/v3/activities/#detailed>
data PolylineDetailed = PolylineDetailed
    { polylineDetailedId              :: Text
    , polylineDetailedPolyline        :: [(Double, Double)]
    , polylineDetailedResourceState   :: Integer
    , polylineDetailedSummaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineDetailed where
    parseJSON (Object o) = do
        id <- o .: "id"
        polyline <- o .: "polyline"
        resourceState <- o .: "resource_state"
        summaryPolyline <- o .:? "summary_polyline"

        return PolylineDetailed
            { polylineDetailedId = id
            , polylineDetailedPolyline = decodeline polyline
            , polylineDetailedResourceState = resourceState
            , polylineDetailedSummaryPolyline = fmap decodeline summaryPolyline
            }

    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#summary>
data PolylineSummary = PolylineSummary
    { polylineSummaryId              :: Text
    , polylineSummaryResourceState   :: Integer
    , polylineSummarySummaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineSummary where
    parseJSON (Object o) = do
        id <- o .: "id"
        resourceState <- o .: "resource_state"
        summaryPolyline <- o .:? "summary_polyline"

        return PolylineSummary
            { polylineSummaryId = id
            , polylineSummaryResourceState = resourceState
            , polylineSummarySummaryPolyline = fmap decodeline summaryPolyline
            }

    parseJSON _ = empty
