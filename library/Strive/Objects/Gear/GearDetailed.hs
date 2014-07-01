{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/gear/#detailed>
module Strive.Objects.Gear.GearDetailed
    ( GearDetailed (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)

-- | Detailed representation of gear.
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
