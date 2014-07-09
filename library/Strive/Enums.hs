{-# LANGUAGE OverloadedStrings #-}

-- | Types for choosing an option from a limited set.
module Strive.Enums where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (String), parseJSON)
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
