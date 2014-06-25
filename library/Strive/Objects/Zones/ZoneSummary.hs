{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#zones>
module Strive.Objects.Zones.ZoneSummary
    ( ZoneSummary (..)
    ) where

import           Control.Applicative    (empty, (<$>), (<*>))
import           Data.Aeson             (FromJSON, Value (Object), parseJSON,
                                         (.:))
import           Data.Text              (Text)
import           Strive.Objects.Buckets (BucketSummary)

-- | Summary representation of a zone.
data ZoneSummary = ZoneSummary
    { distributionBuckets :: [BucketSummary]
    , resourceState       :: Integer
    , sensorBased         :: Bool
    , type_               :: Text
    } deriving Show

instance FromJSON ZoneSummary where
    parseJSON (Object o) = ZoneSummary
        <$> o .: "distribution_buckets"
        <*> o .: "resource_state"
        <*> o .: "sensor_based"
        <*> o .: "type"
    parseJSON _ = empty
