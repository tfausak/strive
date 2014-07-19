{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/clubs/>
module Strive.Types.Clubs
  ( ClubDetailed (..)
  , ClubSummary (..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Enums (ClubType, ResourceState, SportType)
import Strive.Internal.TH (options)

-- | <http://strava.github.io/api/v3/clubs/#detailed>
data ClubDetailed = ClubDetailed
  { clubDetailed_city          :: Text
  , clubDetailed_clubType      :: ClubType
  , clubDetailed_country       :: Text
  , clubDetailed_description   :: Text
  , clubDetailed_id            :: Integer
  , clubDetailed_memberCount   :: Integer
  , clubDetailed_name          :: Text
  , clubDetailed_private       :: Bool
  , clubDetailed_profile       :: Text
  , clubDetailed_profileMedium :: Text
  , clubDetailed_resourceState :: ResourceState
  , clubDetailed_sportType     :: SportType
  , clubDetailed_state         :: Text
  } deriving Show

$(deriveFromJSON options ''ClubDetailed)

-- | <http://strava.github.io/api/v3/clubs/#summary>
data ClubSummary = ClubSummary
  { clubSummary_id            :: Integer
  , clubSummary_name          :: Text
  , clubSummary_profile       :: Text
  , clubSummary_profileMedium :: Text
  , clubSummary_resourceState :: ResourceState
  } deriving Show

$(deriveFromJSON options ''ClubSummary)
