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
    { _effortDetailed_activityId       :: Integer
    , _effortDetailed_athleteId        :: Integer
    , _effortDetailed_averageCadence   :: Maybe Double
    , _effortDetailed_averageHeartrate :: Maybe Double
    , _effortDetailed_averageWatts     :: Maybe Double
    , _effortDetailed_distance         :: Double
    , _effortDetailed_elapsedTime      :: Integer
    , _effortDetailed_endIndex         :: Integer
    , _effortDetailed_hidden           :: Maybe Bool
    , _effortDetailed_id               :: Integer
    , _effortDetailed_komRank          :: Maybe Integer
    , _effortDetailed_max_heartrate    :: Maybe Integer
    , _effortDetailed_movingTime       :: Integer
    , _effortDetailed_name             :: Text
    , _effortDetailed_prRank           :: Maybe Integer
    , _effortDetailed_resourceState    :: Integer
    , _effortDetailed_segment          :: SegmentSummary
    , _effortDetailed_startDate        :: UTCTime
    , _effortDetailed_startDateLocal   :: UTCTime
    , _effortDetailed_startIndex       :: Integer
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
