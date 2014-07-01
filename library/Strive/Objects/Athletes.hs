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
    { athleteDetailedBikes                 :: [GearSummary]
    , athleteDetailedCity                  :: Text
    , athleteDetailedClubs                 :: [ClubSummary]
    , athleteDetailedCountry               :: Text
    , athleteDetailedCreatedAt             :: UTCTime
    , athleteDetailedDatePreference        :: Text
    , athleteDetailedEmail                 :: Text
    , athleteDetailedFirstname             :: Text
    , athleteDetailedFollower              :: Maybe Text
    , athleteDetailedFollowerCount         :: Integer
    , athleteDetailedFriend                :: Maybe Text
    , athleteDetailedFriendCount           :: Integer
    , athleteDetailedFtp                   :: Maybe Integer
    , athleteDetailedId                    :: Integer
    , athleteDetailedLastname              :: Text
    , athleteDetailedMeasurementPreference :: Text
    , athleteDetailedMutualFriendCount     :: Integer
    , athleteDetailedPremium               :: Bool
    , athleteDetailedProfile               :: Text
    , athleteDetailedProfileMedium         :: Text
    , athleteDetailedResourceState         :: Integer
    , athleteDetailedSex                   :: Maybe Char
    , athleteDetailedShoes                 :: [GearSummary]
    , athleteDetailedState                 :: Text
    , athleteDetailedUpdatedAt             :: UTCTime
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
    { athleteSummaryCity          :: Maybe Text
    , athleteSummaryCountry       :: Maybe Text
    , athleteSummaryCreatedAt     :: UTCTime
    , athleteSummaryFirstname     :: Text
    , athleteSummaryFollower      :: Maybe Text
    , athleteSummaryFriend        :: Maybe Text
    , athleteSummaryId            :: Integer
    , athleteSummaryLastname      :: Text
    , athleteSummaryPremium       :: Bool
    , athleteSummaryProfile       :: Text
    , athleteSummaryProfileMedium :: Text
    , athleteSummaryResourceState :: Integer
    , athleteSummarySex           :: Maybe Char
    , athleteSummaryState         :: Text
    , athleteSummaryUpdatedAt     :: UTCTime
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
    { athleteMetaId            :: Integer
    , athleteMetaResourceState :: Integer
    } deriving Show

instance FromJSON AthleteMeta where
    parseJSON (Object o) = AthleteMeta
        <$> o .: "id"
        <*> o .: "resource_state"
    parseJSON _ = empty
