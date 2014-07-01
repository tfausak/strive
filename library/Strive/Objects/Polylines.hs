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
    { _polylineDetailed_id              :: Text
    , _polylineDetailed_polyline        :: [(Double, Double)]
    , _polylineDetailed_resourceState   :: Integer
    , _polylineDetailed_summaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineDetailed where
    parseJSON (Object o) = do
        id <- o .: "id"
        polyline <- o .: "polyline"
        resourceState <- o .: "resource_state"
        summaryPolyline <- o .:? "summary_polyline"

        return PolylineDetailed
            { _polylineDetailed_id = id
            , _polylineDetailed_polyline = decodeline polyline
            , _polylineDetailed_resourceState = resourceState
            , _polylineDetailed_summaryPolyline = fmap decodeline summaryPolyline
            }

    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#summary>
data PolylineSummary = PolylineSummary
    { _polylineSummary_id              :: Text
    , _polylineSummary_resourceState   :: Integer
    , _polylineSummary_summaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineSummary where
    parseJSON (Object o) = do
        id <- o .: "id"
        resourceState <- o .: "resource_state"
        summaryPolyline <- o .:? "summary_polyline"

        return PolylineSummary
            { _polylineSummary_id = id
            , _polylineSummary_resourceState = resourceState
            , _polylineSummary_summaryPolyline = fmap decodeline summaryPolyline
            }

    parseJSON _ = empty
