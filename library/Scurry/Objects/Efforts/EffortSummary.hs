{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/efforts/#summary>
module Scurry.Objects.Efforts.EffortSummary
    ( EffortSummary (..)
    ) where

import           Control.Applicative     (empty, (<$>), (<*>))
import           Data.Aeson              (FromJSON, Value (Object), parseJSON,
                                          (.:), (.:?))
import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime)
import           Scurry.Objects.Segments (SegmentSummary)

-- | Summary representation of an effort.
data EffortSummary = EffortSummary
    { activityId       :: Integer
    , athleteId        :: Integer
    , averageCadence   :: Maybe Double
    , averageHeartrate :: Maybe Double
    , averageWatts     :: Maybe Double
    , distance         :: Double
    , elapsedTime      :: Integer
    , endIndex         :: Integer
    , hidden           :: Maybe Bool
    , id               :: Integer
    , komRank          :: Maybe Integer
    , max_heartrate    :: Maybe Integer
    , movingTime       :: Integer
    , name             :: Text
    , prRank           :: Maybe Integer
    , resourceState    :: Integer
    , segment          :: SegmentSummary
    , startDate        :: UTCTime
    , startDateLocal   :: UTCTime
    , startIndex       :: Integer
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
