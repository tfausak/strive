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
    { _gearDetailed_brandName     :: Text
    , _gearDetailed_description   :: Text
    , _gearDetailed_distance      :: Double
    , _gearDetailed_frameType     :: Maybe Integer
    , _gearDetailed_id            :: Text
    , _gearDetailed_modelName     :: Text
    , _gearDetailed_name          :: Text
    , _gearDetailed_primary       :: Bool
    , _gearDetailed_resourceState :: Integer
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
    { _gearSummary_distance      :: Double
    , _gearSummary_id            :: Text
    , _gearSummary_name          :: Text
    , _gearSummary_primary       :: Bool
    , _gearSummary_resourceState :: Integer
    } deriving Show

instance FromJSON GearSummary where
    parseJSON (Object o) = GearSummary
        <$> o .: "distance"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "primary"
        <*> o .: "resource_state"
    parseJSON _ = empty
