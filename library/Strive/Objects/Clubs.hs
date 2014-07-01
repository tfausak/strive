{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/clubs/>
module Strive.Objects.Clubs
    ( ClubDetailed (..)
    , ClubSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/clubs/#detailed>
data ClubDetailed = ClubDetailed
    { clubDetailedCity          :: Text
    , clubDetailedClubType      :: Text
    , clubDetailedCountry       :: Text
    , clubDetailedDescription   :: Text
    , clubDetailedId            :: Integer
    , clubDetailedMemberCount   :: Integer
    , clubDetailedName          :: Text
    , clubDetailedPrivate       :: Bool
    , clubDetailedProfile       :: Text
    , clubDetailedProfileMedium :: Text
    , clubDetailedResourceState :: Integer
    , clubDetailedSportType     :: Text
    , clubDetailedState         :: Text
    } deriving Show

instance FromJSON ClubDetailed where
    parseJSON (Object o) = ClubDetailed
        <$> o .: "city"
        <*> o .: "club_type"
        <*> o .: "country"
        <*> o .: "description"
        <*> o .: "id"
        <*> o .: "member_count"
        <*> o .: "name"
        <*> o .: "private"
        <*> o .: "profile"
        <*> o .: "profile_medium"
        <*> o .: "resource_state"
        <*> o .: "sport_type"
        <*> o .: "state"
    parseJSON _ = empty

-- | <http://strava.github.io/api/v3/clubs/#summary>
data ClubSummary = ClubSummary
    { clubSummaryId            :: Integer
    , clubSummaryName          :: Text
    , clubSummaryProfile       :: Text
    , clubSummaryProfileMedium :: Text
    , clubSummaryResourceState :: Integer
    } deriving Show

instance FromJSON ClubSummary where
    parseJSON (Object o) = ClubSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "profile"
        <*> o .: "profile_medium"
        <*> o .: "resource_state"
    parseJSON _ = empty
