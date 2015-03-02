{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/#polylines>
module Strive.Types.Polylines
  ( Polyline (..)
  , PolylineDetailed (..)
  , PolylineSummary (..)
  ) where

import Data.Aeson (FromJSON, parseJSON)
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import GPolyline (decodeline)
import Strive.Enums (ResourceState)
import Strive.Internal.TH (options)

newtype Polyline = Polyline { unPolyline :: [(Double, Double)] } deriving Show

instance FromJSON Polyline where
  parseJSON = fmap (Polyline . decodeline) . parseJSON

-- | <http://strava.github.io/api/v3/activities/#detailed>
data PolylineDetailed = PolylineDetailed
  { polylineDetailed_id              :: Text
  , polylineDetailed_polyline        :: Polyline
  , polylineDetailed_resourceState   :: ResourceState
  , polylineDetailed_summaryPolyline :: Maybe Polyline
  } deriving Show

$(deriveFromJSON options ''PolylineDetailed)

-- | <http://strava.github.io/api/v3/activities/#summary>
data PolylineSummary = PolylineSummary
  { polylineSummary_id              :: Text
  , polylineSummary_resourceState   :: ResourceState
  , polylineSummary_summaryPolyline :: Maybe Polyline
  } deriving Show

$(deriveFromJSON options ''PolylineSummary)
