{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/gear/>
module Strive.Objects.Gear
    ( GearDetailed (..)
    , GearSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/gear/#detailed>
data GearDetailed = GearDetailed
    { gearDetailedBrandName     :: Text
    , gearDetailedDescription   :: Text
    , gearDetailedDistance      :: Double
    , gearDetailedFrameType     :: Maybe Integer
    , gearDetailedId            :: Text
    , gearDetailedModelName     :: Text
    , gearDetailedName          :: Text
    , gearDetailedPrimary       :: Bool
    , gearDetailedResourceState :: Integer
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
    { gearSummaryDistance      :: Double
    , gearSummaryId            :: Text
    , gearSummaryName          :: Text
    , gearSummaryPrimary       :: Bool
    , gearSummaryResourceState :: Integer
    } deriving Show

instance FromJSON GearSummary where
    parseJSON (Object o) = GearSummary
        <$> o .: "distance"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "primary"
        <*> o .: "resource_state"
    parseJSON _ = empty
