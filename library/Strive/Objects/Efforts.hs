{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Objects.Efforts
    ( EffortSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Segments (SegmentSummary)

-- | <http://strava.github.io/api/v3/efforts/#summary>
data EffortSummary = EffortSummary
    { effortSummaryActivityId       :: Integer
    , effortSummaryAthleteId        :: Integer
    , effortSummaryAverageCadence   :: Maybe Double
    , effortSummaryAverageHeartrate :: Maybe Double
    , effortSummaryAverageWatts     :: Maybe Double
    , effortSummaryDistance         :: Double
    , effortSummaryElapsedTime      :: Integer
    , effortSummaryEndIndex         :: Integer
    , effortSummaryHidden           :: Maybe Bool
    , effortSummaryId               :: Integer
    , effortSummaryKomRank          :: Maybe Integer
    , effortSummaryMax_heartrate    :: Maybe Integer
    , effortSummaryMovingTime       :: Integer
    , effortSummaryName             :: Text
    , effortSummaryPrRank           :: Maybe Integer
    , effortSummaryResourceState    :: Integer
    , effortSummarySegment          :: SegmentSummary
    , effortSummaryStartDate        :: UTCTime
    , effortSummaryStartDateLocal   :: UTCTime
    , effortSummaryStartIndex       :: Integer
    } deriving Show

instance FromJSON EffortSummary where
    parseJSON (Object o) = EffortSummary
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
