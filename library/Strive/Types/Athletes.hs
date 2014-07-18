{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Athletes
  ( AthleteDetailed (..)
  , AthleteSummary (..)
  , AthleteMeta (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (Gender, MeasurementPreference, ResourceState)
import Strive.Types.Clubs (ClubSummary)
import Strive.Types.Gear (GearSummary)

-- | <http://strava.github.io/api/v3/athlete/#detailed>
data AthleteDetailed = AthleteDetailed
  { athleteDetailed_bikes                 :: [GearSummary]
  , athleteDetailed_city                  :: Text
  , athleteDetailed_clubs                 :: [ClubSummary]
  , athleteDetailed_country               :: Text
  , athleteDetailed_createdAt             :: UTCTime
  , athleteDetailed_datePreference        :: Text
  , athleteDetailed_email                 :: Text
  , athleteDetailed_firstname             :: Text
  , athleteDetailed_follower              :: Maybe Text
  , athleteDetailed_followerCount         :: Integer
  , athleteDetailed_friend                :: Maybe Text
  , athleteDetailed_friendCount           :: Integer
  , athleteDetailed_ftp                   :: Maybe Integer
  , athleteDetailed_id                    :: Integer
  , athleteDetailed_lastname              :: Text
  , athleteDetailed_measurementPreference :: MeasurementPreference
  , athleteDetailed_mutualFriendCount     :: Integer
  , athleteDetailed_premium               :: Bool
  , athleteDetailed_profile               :: Text
  , athleteDetailed_profileMedium         :: Text
  , athleteDetailed_resourceState         :: ResourceState
  , athleteDetailed_sex                   :: Maybe Gender
  , athleteDetailed_shoes                 :: [GearSummary]
  , athleteDetailed_state                 :: Text
  , athleteDetailed_updatedAt             :: UTCTime
  } deriving Show

instance FromJSON AthleteDetailed where
  parseJSON (Object o) = AthleteDetailed
    <$> o .: "bikes"
    <*> o .: "city"
    <*> o .: "clubs"
    <*> o .: "country"
    <*> o .: "created_at"
    <*> o .: "date_preference"
    <*> o .: "email"
    <*> o .: "firstname"
    <*> o .:? "follower"
    <*> o .: "follower_count"
    <*> o .:? "friend"
    <*> o .: "friend_count"
    <*> o .:? "ftp"
    <*> o .: "id"
    <*> o .: "lastname"
    <*> o .: "measurement_preference"
    <*> o .: "mutual_friend_count"
    <*> o .: "premium"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .:? "sex"
    <*> o .: "shoes"
    <*> o .: "state"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/athlete/#summary>
data AthleteSummary = AthleteSummary
  { athleteSummary_city          :: Maybe Text
  , athleteSummary_country       :: Maybe Text
  , athleteSummary_createdAt     :: UTCTime
  , athleteSummary_firstname     :: Text
  , athleteSummary_follower      :: Maybe Text
  , athleteSummary_friend        :: Maybe Text
  , athleteSummary_id            :: Integer
  , athleteSummary_lastname      :: Text
  , athleteSummary_premium       :: Bool
  , athleteSummary_profile       :: Text
  , athleteSummary_profileMedium :: Text
  , athleteSummary_resourceState :: ResourceState
  , athleteSummary_sex           :: Maybe Gender
  , athleteSummary_state         :: Text
  , athleteSummary_updatedAt     :: UTCTime
  } deriving Show

instance FromJSON AthleteSummary where
  parseJSON (Object o) = AthleteSummary
    <$> o .:? "city"
    <*> o .:? "country"
    <*> o .: "created_at"
    <*> o .: "firstname"
    <*> o .:? "follower"
    <*> o .:? "friend"
    <*> o .: "id"
    <*> o .: "lastname"
    <*> o .: "premium"
    <*> o .: "profile"
    <*> o .: "profile_medium"
    <*> o .: "resource_state"
    <*> o .:? "sex"
    <*> o .: "state"
    <*> o .: "updated_at"
  parseJSON _ = empty

-- | <http://strava.github.io/api/v3/athlete/#meta>
data AthleteMeta = AthleteMeta
  { athleteMeta_id            :: Integer
  , athleteMeta_resourceState :: ResourceState
  } deriving Show

instance FromJSON AthleteMeta where
  parseJSON (Object o) = AthleteMeta
    <$> o .: "id"
    <*> o .: "resource_state"
  parseJSON _ = empty
