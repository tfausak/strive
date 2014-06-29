{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/segments/#detailed>
module Strive.Objects.Polylines.PolylineSummary
    ( PolylineSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import GPolyline (decodeline)
import Prelude hiding (id)

-- | Detailed representation of a polyline.
data PolylineSummary = PolylineSummary
    { id              :: Text
    , resourceState   :: Integer
    , summaryPolyline :: Maybe [(Double, Double)]
    } deriving Show

instance FromJSON PolylineSummary where
    parseJSON (Object o) = do
        id' <- o .: "id"
        resourceState' <- o .: "resource_state"
        summaryPolyline' <- o .:? "summary_polyline"

        return PolylineSummary
            { id = id'
            , resourceState = resourceState'
            , summaryPolyline = fmap decodeline summaryPolyline'
            }

    parseJSON _ = empty
