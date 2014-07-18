{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Gear
  ( GearDetailed (..)
  , GearSummary (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Strive.Enums (FrameType, ResourceState)

-- | <http://strava.github.io/api/v3/gear/#detailed>
data GearDetailed = GearDetailed
  { gearDetailed_brandName     :: Text
  , gearDetailed_description   :: Text
  , gearDetailed_distance      :: Double
  , gearDetailed_frameType     :: Maybe FrameType
  , gearDetailed_id            :: Text
  , gearDetailed_modelName     :: Text
  , gearDetailed_name          :: Text
  , gearDetailed_primary       :: Bool
  , gearDetailed_resourceState :: ResourceState
  } deriving Show

instance FromJSON GearDetailed where
  parseJSON (Object o) = GearDetailed
    <$> o .: "brand_name"
    <*> o .: "description"
    <*> o .: "distance"
    <*> o .:? "frame_type"
    <*> o .: "id"
    <*> o .: "model_name"
    <*> o .: "name"
    <*> o .: "primary"
    <*> o .: "resource_state"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/gear/#summary>
data GearSummary = GearSummary
  { gearSummary_distance      :: Double
  , gearSummary_id            :: Text
  , gearSummary_name          :: Text
  , gearSummary_primary       :: Bool
  , gearSummary_resourceState :: ResourceState
  } deriving Show

instance FromJSON GearSummary where
  parseJSON (Object o) = GearSummary
    <$> o .: "distance"
    <*> o .: "id"
    <*> o .: "name"
    <*> o .: "primary"
    <*> o .: "resource_state"
  parseJSON _ = empty
