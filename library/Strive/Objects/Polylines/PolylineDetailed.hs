{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#detailed>
module Strive.Objects.Polylines.PolylineDetailed
    ( PolylineDetailed (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import GPolyline (decodeline)
import Prelude hiding (id)

-- | Detailed representation of a polyline.
data PolylineDetailed = PolylineDetailed
    { id              :: Text
    , polyline        :: [(Double, Double)]
    , resourceState   :: Integer
    , summaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineDetailed where
    parseJSON (Object o) = do
        id' <- o .: "id"
        polyline' <- o .: "polyline"
        resourceState' <- o .: "resource_state"
        summaryPolyline' <- o .:? "summary_polyline"

        return PolylineDetailed
            { id = id'
            , polyline = decodeline polyline'
            , resourceState = resourceState'
            , summaryPolyline = fmap decodeline summaryPolyline'
            }

    parseJSON _ = empty
