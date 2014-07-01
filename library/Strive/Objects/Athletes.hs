{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/athlete/>
module Strive.Objects.Athletes
    ( AthleteDetailed (..)
    , AthleteSummary (..)
    , AthleteMeta (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Clubs (ClubSummary)
import Strive.Objects.Gear (GearSummary)

-- | <http://strava.github.io/api/v3/athlete/#detailed>
data AthleteDetailed = AthleteDetailed
    { _athleteDetailed_bikes                 :: [GearSummary]
    , _athleteDetailed_city                  :: Text
    , _athleteDetailed_clubs                 :: [ClubSummary]
    , _athleteDetailed_country               :: Text
    , _athleteDetailed_createdAt             :: UTCTime
    , _athleteDetailed_datePreference        :: Text
    , _athleteDetailed_email                 :: Text
    , _athleteDetailed_firstname             :: Text
    , _athleteDetailed_follower              :: Maybe Text
    , _athleteDetailed_followerCount         :: Integer
    , _athleteDetailed_friend                :: Maybe Text
    , _athleteDetailed_friendCount           :: Integer
    , _athleteDetailed_ftp                   :: Maybe Integer
    , _athleteDetailed_id                    :: Integer
    , _athleteDetailed_lastname              :: Text
    , _athleteDetailed_measurementPreference :: Text
    , _athleteDetailed_mutualFriendCount     :: Integer
    , _athleteDetailed_premium               :: Bool
    , _athleteDetailed_profile               :: Text
    , _athleteDetailed_profileMedium         :: Text
    , _athleteDetailed_resourceState         :: Integer
    , _athleteDetailed_sex                   :: Maybe Char
    , _athleteDetailed_shoes                 :: [GearSummary]
    , _athleteDetailed_state                 :: Text
    , _athleteDetailed_updatedAt             :: UTCTime
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
    { _athleteSummary_city          :: Maybe Text
    , _athleteSummary_country       :: Maybe Text
    , _athleteSummary_createdAt     :: UTCTime
    , _athleteSummary_firstname     :: Text
    , _athleteSummary_follower      :: Maybe Text
    , _athleteSummary_friend        :: Maybe Text
    , _athleteSummary_id            :: Integer
    , _athleteSummary_lastname      :: Text
    , _athleteSummary_premium       :: Bool
    , _athleteSummary_profile       :: Text
    , _athleteSummary_profileMedium :: Text
    , _athleteSummary_resourceState :: Integer
    , _athleteSummary_sex           :: Maybe Char
    , _athleteSummary_state         :: Text
    , _athleteSummary_updatedAt     :: UTCTime
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
    { _athleteMeta_id            :: Integer
    , _athleteMeta_resourceState :: Integer
    } deriving Show

instance FromJSON AthleteMeta where
    parseJSON (Object o) = AthleteMeta
        <$> o .: "id"
        <*> o .: "resource_state"
    parseJSON _ = empty
