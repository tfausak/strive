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
    { clubDetailed_city          :: Text
    , clubDetailed_clubType      :: Text
    , clubDetailed_country       :: Text
    , clubDetailed_description   :: Text
    , clubDetailed_id            :: Integer
    , clubDetailed_memberCount   :: Integer
    , clubDetailed_name          :: Text
    , clubDetailed_private       :: Bool
    , clubDetailed_profile       :: Text
    , clubDetailed_profileMedium :: Text
    , clubDetailed_resourceState :: Integer
    , clubDetailed_sportType     :: Text
    , clubDetailed_state         :: Text
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
    { clubSummary_id            :: Integer
    , clubSummary_name          :: Text
    , clubSummary_profile       :: Text
    , clubSummary_profileMedium :: Text
    , clubSummary_resourceState :: Integer
    } deriving Show

instance FromJSON ClubSummary where
    parseJSON (Object o) = ClubSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "profile"
        <*> o .: "profile_medium"
        <*> o .: "resource_state"
    parseJSON _ = empty
