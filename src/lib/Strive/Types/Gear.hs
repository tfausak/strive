{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/gear/>
module Strive.Types.Gear
  ( GearDetailed (..)
  , GearSummary (..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Enums (FrameType, ResourceState)
import Strive.Internal.TH (options)

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

$(deriveFromJSON options ''GearDetailed)

-- | <http://strava.github.io/api/v3/gear/#summary>
data GearSummary = GearSummary
  { gearSummary_distance      :: Double
  , gearSummary_id            :: Text
  , gearSummary_name          :: Text
  , gearSummary_primary       :: Bool
  , gearSummary_resourceState :: ResourceState
  } deriving Show

$(deriveFromJSON options ''GearSummary)
