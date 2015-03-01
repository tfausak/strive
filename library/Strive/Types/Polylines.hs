{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/#polylines>
module Strive.Types.Polylines
  ( Polyline (..)
  , PolylineDetailed (..)
  , PolylineSummary (..)
  ) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import GPolyline (decodeline)
import Strive.Enums (ResourceState)

newtype Polyline = Polyline { unPolyline :: [(Double, Double)] } deriving Show

instance FromJSON Polyline where
  parseJSON = fmap (Polyline . decodeline) . parseJSON

-- | <http://strava.github.io/api/v3/activities/#detailed>
data PolylineDetailed = PolylineDetailed
  { polylineDetailed_id              :: Text
  , polylineDetailed_polyline        :: [(Double, Double)]
  , polylineDetailed_resourceState   :: ResourceState
  , polylineDetailed_summaryPolyline :: Maybe [(Double, Double)]
  } deriving Show

instance FromJSON PolylineDetailed where
  parseJSON (Object o) = do
    id_ <- o .: "id"
    polyline <- o .: "polyline"
    resourceState <- o .: "resource_state"
    summaryPolyline <- o .:? "summary_polyline"

    return PolylineDetailed
      { polylineDetailed_id = id_
      , polylineDetailed_polyline = decodeline polyline
      , polylineDetailed_resourceState = resourceState
      , polylineDetailed_summaryPolyline = fmap decodeline summaryPolyline
      }

  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/activities/#summary>
data PolylineSummary = PolylineSummary
  { polylineSummary_id              :: Text
  , polylineSummary_resourceState   :: ResourceState
  , polylineSummary_summaryPolyline :: Maybe [(Double, Double)]
  } deriving Show

instance FromJSON PolylineSummary where
  parseJSON (Object o) = do
    id_ <- o .: "id"
    resourceState <- o .: "resource_state"
    summaryPolyline <- o .:? "summary_polyline"

    return PolylineSummary
      { polylineSummary_id = id_
      , polylineSummary_resourceState = resourceState
      , polylineSummary_summaryPolyline = fmap decodeline summaryPolyline
      }

  parseJSON _ = empty
