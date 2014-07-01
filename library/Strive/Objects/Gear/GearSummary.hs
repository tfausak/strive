{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/gear/#summary>
module Strive.Objects.Gear.GearSummary
    ( GearSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)

-- | Summary representation of gear.
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
