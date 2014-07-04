{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Objects.Efforts
    ( EffortDetailed (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Segments (SegmentSummary)

-- | <http://strava.github.io/api/v3/efforts/#detailed>
data EffortDetailed = EffortDetailed
    { effortDetailedActivityId       :: Integer
    , effortDetailedAthleteId        :: Integer
    , effortDetailedAverageCadence   :: Maybe Double
    , effortDetailedAverageHeartrate :: Maybe Double
    , effortDetailedAverageWatts     :: Maybe Double
    , effortDetailedDistance         :: Double
    , effortDetailedElapsedTime      :: Integer
    , effortDetailedEndIndex         :: Integer
    , effortDetailedHidden           :: Maybe Bool
    , effortDetailedId               :: Integer
    , effortDetailedKomRank          :: Maybe Integer
    , effortDetailedMaxHeartrate     :: Maybe Integer
    , effortDetailedMovingTime       :: Integer
    , effortDetailedName             :: Text
    , effortDetailedPrRank           :: Maybe Integer
    , effortDetailedResourceState    :: Integer
    , effortDetailedSegment          :: SegmentSummary
    , effortDetailedStartDate        :: UTCTime
    , effortDetailedStartDateLocal   :: UTCTime
    , effortDetailedStartIndex       :: Integer
    } deriving Show

instance FromJSON EffortDetailed where
    parseJSON (Object o) = EffortDetailed
        <$> ((o .: "activity") >>= (.: "id"))
        <*> ((o .: "athlete") >>= (.: "id"))
        <*> o .:? "average_cadence"
        <*> o .:? "average_heartrate"
        <*> o .:? "average_watts"
        <*> o .: "distance"
        <*> o .: "elapsed_time"
        <*> o .: "end_index"
        <*> o .:? "hidden"
        <*> o .: "id"
        <*> o .:? "kom_rank"
        <*> o .:? "max_heartrate"
        <*> o .: "moving_time"
        <*> o .: "name"
        <*> o .:? "pr_rank"
        <*> o .: "resource_state"
        <*> o .: "segment"
        <*> o .: "start_date"
        <*> o .: "start_date_local"
        <*> o .: "start_index"
    parseJSON _ = empty
