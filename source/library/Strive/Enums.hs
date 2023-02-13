{-# LANGUAGE OverloadedStrings #-}

-- | Types for choosing an option from a limited set.
module Strive.Enums
  ( ActivityType (..),
    ActivityZoneType (..),
    AgeGroup (..),
    ClubType (..),
    FrameType (..),
    Gender (..),
    MeasurementPreference (..),
    PhotoType (..),
    Resolution (..),
    ResourceState (..),
    SegmentActivityType (..),
    SeriesType (..),
    SportType (..),
    StreamType (..),
    WeightClass (..),
  )
where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (Number, String), parseJSON)

-- | An activity's type.
data ActivityType
  = AlpineSki
  | BackcountrySki
  | Canoeing
  | CrossCountrySkiing
  | Crossfit
  | Elliptical
  | Hike
  | IceSkate
  | InlineSkate
  | Kayaking
  | KiteSurf
  | NordicSki
  | Ride
  | RockClimbing
  | RollerSki
  | Rowing
  | Run
  | Snowboard
  | Snowshoe
  | StairStepper
  | StandUpPaddling
  | Surfing
  | Swim
  | VirtualRide
  | Walk
  | WeightTraining
  | Windsurf
  | Workout
  | Yoga
  deriving (Eq, Show)

instance FromJSON ActivityType where
  parseJSON (String "AlpineSki") = pure AlpineSki
  parseJSON (String "BackcountrySki") = pure BackcountrySki
  parseJSON (String "Canoeing") = pure Canoeing
  parseJSON (String "CrossCountrySkiing") = pure CrossCountrySkiing
  parseJSON (String "Crossfit") = pure Crossfit
  parseJSON (String "Elliptical") = pure Elliptical
  parseJSON (String "Hike") = pure Hike
  parseJSON (String "IceSkate") = pure IceSkate
  parseJSON (String "InlineSkate") = pure InlineSkate
  parseJSON (String "Kayaking") = pure Kayaking
  parseJSON (String "KiteSurf") = pure KiteSurf
  parseJSON (String "NordicSki") = pure NordicSki
  parseJSON (String "Ride") = pure Ride
  parseJSON (String "RockClimbing") = pure RockClimbing
  parseJSON (String "RollerSki") = pure RollerSki
  parseJSON (String "Rowing") = pure Rowing
  parseJSON (String "Run") = pure Run
  parseJSON (String "Snowboard") = pure Snowboard
  parseJSON (String "Snowshoe") = pure Snowshoe
  parseJSON (String "StairStepper") = pure StairStepper
  parseJSON (String "StandUpPaddling") = pure StandUpPaddling
  parseJSON (String "Surfing") = pure Surfing
  parseJSON (String "Swim") = pure Swim
  parseJSON (String "VirtualRide") = pure VirtualRide
  parseJSON (String "Walk") = pure Walk
  parseJSON (String "WeightTraining") = pure WeightTraining
  parseJSON (String "Windsurf") = pure Windsurf
  parseJSON (String "Workout") = pure Workout
  parseJSON (String "Yoga") = pure Yoga
  parseJSON _ = empty

-- | An activity zone's type.
data ActivityZoneType
  = HeartrateZone
  | PowerZone
  deriving (Eq, Show)

instance FromJSON ActivityZoneType where
  parseJSON (String "heartrate") = pure HeartrateZone
  parseJSON (String "power") = pure PowerZone
  parseJSON _ = empty

-- | An athlete's age group.
data AgeGroup
  = Ages0To24
  | Ages25To34
  | Ages35To44
  | Ages45To54
  | Ages55To64
  | Ages65Plus
  deriving (Eq)

instance Show AgeGroup where
  show Ages0To24 = "0_24"
  show Ages25To34 = "25_34"
  show Ages35To44 = "35_44"
  show Ages45To54 = "45_54"
  show Ages55To64 = "55_64"
  show Ages65Plus = "65_plus"

-- | A club's type.
data ClubType
  = CasualClub
  | Company
  | Other
  | RacingTeam
  | Shop
  deriving (Eq, Show)

instance FromJSON ClubType where
  parseJSON (String "casual_club") = pure CasualClub
  parseJSON (String "company") = pure Company
  parseJSON (String "other") = pure Other
  parseJSON (String "racing_team") = pure RacingTeam
  parseJSON (String "shop") = pure Shop
  parseJSON _ = empty

-- | A bike's frame type.
data FrameType
  = CrossFrame
  | MountainFrame
  | RoadFrame
  | TimeTrialFrame
  deriving (Eq, Show)

instance FromJSON FrameType where
  parseJSON (Number 2) = pure CrossFrame
  parseJSON (Number 1) = pure MountainFrame
  parseJSON (Number 3) = pure RoadFrame
  parseJSON (Number 4) = pure TimeTrialFrame
  parseJSON _ = empty

-- | An athlete's gender.
data Gender
  = Female
  | Male
  deriving (Eq)

instance Show Gender where
  show Female = "F"
  show Male = "M"

instance FromJSON Gender where
  parseJSON (String "F") = pure Female
  parseJSON (String "M") = pure Male
  parseJSON _ = empty

-- | An athlete's measurement preference.
data MeasurementPreference
  = Feet
  | Meters
  deriving (Eq, Show)

instance FromJSON MeasurementPreference where
  parseJSON (String "feet") = pure Feet
  parseJSON (String "meters") = pure Meters
  parseJSON _ = empty

-- | A photo's type.
data PhotoType = InstagramPhoto
  deriving (Eq, Show)

instance FromJSON PhotoType where
  parseJSON (String "InstagramPhoto") = pure InstagramPhoto
  parseJSON _ = empty

-- | A stream's resolution.
data Resolution
  = Low
  | Medium
  | High
  deriving (Eq)

instance Show Resolution where
  show Low = "low"
  show Medium = "medium"
  show High = "high"

instance FromJSON Resolution where
  parseJSON (String "low") = pure Low
  parseJSON (String "medium") = pure Medium
  parseJSON (String "high") = pure High
  parseJSON _ = empty

-- | A resource's state.
data ResourceState
  = Meta
  | Summary
  | Detailed
  deriving (Eq, Show)

instance FromJSON ResourceState where
  parseJSON (Number 1) = pure Meta
  parseJSON (Number 2) = pure Summary
  parseJSON (Number 3) = pure Detailed
  parseJSON _ = empty

-- | A segment's activity type.
data SegmentActivityType
  = Riding
  | Running
  deriving (Eq)

instance Show SegmentActivityType where
  show Riding = "riding"
  show Running = "running"

-- | A series' type in a stream.
data SeriesType
  = Distance
  | Time
  deriving (Eq)

instance Show SeriesType where
  show Distance = "distance"
  show Time = "time"

instance FromJSON SeriesType where
  parseJSON (String "distance") = pure Distance
  parseJSON (String "time") = pure Time
  parseJSON _ = empty

-- | A club's sport type.
data SportType
  = SportCycling
  | SportOther
  | SportRunning
  | SportTriathalon
  deriving (Eq, Show)

instance FromJSON SportType where
  parseJSON (String "cycling") = pure SportCycling
  parseJSON (String "other") = pure SportOther
  parseJSON (String "running") = pure SportRunning
  parseJSON (String "triathalon") = pure SportTriathalon
  parseJSON _ = empty

-- | A stream's type.
data StreamType
  = AltitudeStream
  | CadenceStream
  | DistanceStream
  | GradeSmoothStream
  | HeartrateStream
  | LatlngStream
  | MovingStream
  | TempStream
  | TimeStream
  | VelocitySmoothStream
  | WattsStream
  deriving (Eq)

instance Show StreamType where
  show AltitudeStream = "altitude"
  show CadenceStream = "cadence"
  show DistanceStream = "distance"
  show GradeSmoothStream = "grade_smooth"
  show HeartrateStream = "heartrate"
  show LatlngStream = "latlng"
  show MovingStream = "moving"
  show TempStream = "temp"
  show TimeStream = "time"
  show VelocitySmoothStream = "velocity_smooth"
  show WattsStream = "watts"

-- | An athlete's weight class.
data WeightClass
  = Kilograms0To54
  | Kilograms55To64
  | Kilograms65To74
  | Kilograms75To84
  | Kilograms85To94
  | Kilograms95Plus
  | Pounds0To124
  | Pounds125To149
  | Pounds150To164
  | Pounds165To179
  | Pounds180To199
  | Pounds200Plus
  deriving (Eq)

instance Show WeightClass where
  show Kilograms0To54 = "0_54"
  show Kilograms55To64 = "55_64"
  show Kilograms65To74 = "65_74"
  show Kilograms75To84 = "75_84"
  show Kilograms85To94 = "85_94"
  show Kilograms95Plus = "95_plus"
  show Pounds0To124 = "0_124"
  show Pounds125To149 = "125_149"
  show Pounds150To164 = "150_164"
  show Pounds165To179 = "165_179"
  show Pounds180To199 = "180_199"
  show Pounds200Plus = "200_plus"
