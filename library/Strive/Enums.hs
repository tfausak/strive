{-# LANGUAGE OverloadedStrings #-}

-- | Types for choosing an option from a limited set.
module Strive.Enums where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (Number, String), parseJSON)
import Data.Char (toLower, toUpper)
import Data.Text (unpack)
import Text.Read (readMaybe)

-- | An athlete's gender.
data Gender = Female | Male

instance FromJSON Gender where
  parseJSON (String "F") = return Female
  parseJSON (String "M") = return Male
  parseJSON _ = empty

instance Show Gender where
  show Female = "F"
  show Male = "M"

-- | An activity's type.
data ActivityType
  = Alpineski
  | Backcountryski
  | Hike
  | Iceskate
  | Inlineskate
  | Kitesurf
  | Nordicski
  | Ride
  | Rollerski
  | Run
  | Snowboard
  | Snowshoe
  | Swim
  | Walk
  | Windsurf
  | Workout
  deriving (Read, Show)

instance FromJSON ActivityType where
  parseJSON (String s) = case readMaybe (capitalize (unpack s)) of
    Just t -> return t
    _ -> empty
   where
    capitalize [] = []
    capitalize (x : xs) = toUpper x : fmap toLower xs
  parseJSON _ = empty

-- | An athlete's age group.
data AgeGroup
  = Ages0To24
  | Ages25To34
  | Ages35To44
  | Ages45To54
  | Ages55To64
  | Ages65Plus

instance Show AgeGroup where
  show Ages0To24 = "0_24"
  show Ages25To34 = "25_34"
  show Ages35To44 = "35_44"
  show Ages45To54 = "45_54"
  show Ages55To64 = "55_64"
  show Ages65Plus = "65_plus"

-- | An athlete's weight class.
data WeightClass
  = Pounds0To124
  | Pounds125To149
  | Pounds150To164
  | Pounds165To179
  | Pounds180To199
  | Pounds200Plus
  | Kilograms0To54
  | Kilograms55To64
  | Kilograms65To74
  | Kilograms75To84
  | Kilograms85To94
  | Kilograms95Plus

instance Show WeightClass where
  show Pounds0To124 = "0_124"
  show Pounds125To149 = "125_149"
  show Pounds150To164 = "150_164"
  show Pounds165To179 = "165_179"
  show Pounds180To199 = "180_199"
  show Pounds200Plus = "200_plus"
  show Kilograms0To54 = "0_54"
  show Kilograms55To64 = "55_64"
  show Kilograms65To74 = "65_74"
  show Kilograms75To84 = "75_84"
  show Kilograms85To94 = "85_94"
  show Kilograms95Plus = "95_plus"

-- | A segment's activity type.
data SegmentActivityType
  = Riding
  | Running

instance Show SegmentActivityType where
  show Riding = "riding"
  show Running = "running"

-- | A stream's resolution.
data Resolution
  = Low
  | Medium
  | High

instance Show Resolution where
  show Low = "low"
  show Medium = "medium"
  show High = "high"

instance FromJSON Resolution where
  parseJSON (String "low") = return Low
  parseJSON (String "medium") = return Medium
  parseJSON (String "high") = return High
  parseJSON _ = empty

-- | A series' type in a stream.
data SeriesType
  = Distance
  | Time

instance Show SeriesType where
  show Distance = "distance"
  show Time = "time"

instance FromJSON SeriesType where
  parseJSON (String "distance") = return Distance
  parseJSON (String "time") = return Time
  parseJSON _ = empty

-- | A resource's state.
data ResourceState
  = Meta
  | Summary
  | Detailed
  deriving Show

instance FromJSON ResourceState where
  parseJSON (Number 1) = return Meta
  parseJSON (Number 2) = return Summary
  parseJSON (Number 3) = return Detailed
  parseJSON _ = empty

-- | An athlete's measurement preference.
data MeasurementPreference
  = Feet
  | Meters
  deriving Show

instance FromJSON MeasurementPreference where
  parseJSON (String "feet") = return Feet
  parseJSON (String "meters") = return Meters
  parseJSON _ = empty
