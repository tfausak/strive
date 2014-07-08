{-# LANGUAGE OverloadedStrings #-}

-- | Types for choosing an option from a limited set.
module Strive.Enums where

import Control.Applicative (empty)
import Data.Aeson (FromJSON, Value (String), parseJSON)

-- | An athlete's gender.
data Gender = Female | Male

instance FromJSON Gender where
  parseJSON (String "F") = return Female
  parseJSON (String "M") = return Male
  parseJSON _ = empty

instance Show Gender where
  show Female = "F"
  show Male = "M"
