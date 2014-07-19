{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/efforts/>
module Strive.Types.Efforts
  ( EffortDetailed (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (PhotoType, ResourceState)
import Strive.Types.Segments (SegmentSummary)

-- | <http://strava.github.io/api/v3/efforts/#detailed>
data EffortDetailed = EffortDetailed
  { effortDetailed_activityId       :: Integer
  , effortDetailed_athleteId        :: Integer
  , effortDetailed_averageCadence   :: Maybe Double
  , effortDetailed_averageHeartrate :: Maybe Double
  , effortDetailed_averageWatts     :: Maybe Double
  , effortDetailed_distance         :: Double
  , effortDetailed_elapsedTime      :: Integer
  , effortDetailed_endIndex         :: Integer
  , effortDetailed_hidden           :: Maybe Bool
  , effortDetailed_id               :: Integer
  , effortDetailed_komRank          :: Maybe Integer
  , effortDetailed_maxHeartrate     :: Maybe Integer
  , effortDetailed_movingTime       :: Integer
  , effortDetailed_name             :: Text
  , effortDetailed_prRank           :: Maybe Integer
  , effortDetailed_resourceState    :: ResourceState
  , effortDetailed_segment          :: SegmentSummary
  , effortDetailed_startDate        :: UTCTime
  , effortDetailed_startDateLocal   :: UTCTime
  , effortDetailed_startIndex       :: Integer
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
