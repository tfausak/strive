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
    { _clubDetailed_city          :: Text
    , _clubDetailed_clubType      :: Text
    , _clubDetailed_country       :: Text
    , _clubDetailed_description   :: Text
    , _clubDetailed_id            :: Integer
    , _clubDetailed_memberCount   :: Integer
    , _clubDetailed_name          :: Text
    , _clubDetailed_private       :: Bool
    , _clubDetailed_profile       :: Text
    , _clubDetailed_profileMedium :: Text
    , _clubDetailed_resourceState :: Integer
    , _clubDetailed_sportType     :: Text
    , _clubDetailed_state         :: Text
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
    { _clubSummary_id            :: Integer
    , _clubSummary_name          :: Text
    , _clubSummary_profile       :: Text
    , _clubSummary_profileMedium :: Text
    , _clubSummary_resourceState :: Integer
    } deriving Show

instance FromJSON ClubSummary where
    parseJSON (Object o) = ClubSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "profile"
        <*> o .: "profile_medium"
        <*> o .: "resource_state"
    parseJSON _ = empty
