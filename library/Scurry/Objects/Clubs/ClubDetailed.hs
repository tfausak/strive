{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/clubs/>
module Scurry.Objects.Clubs.ClubDetailed
    ( ClubDetailed (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)

-- | <http://strava.github.io/api/v3/clubs/#detailed-representation-attributes-a-iddetailednbspa>
data ClubDetailed = ClubDetailed
    { city          :: Text
    , clubType      :: Text
    , country       :: Text
    , description   :: Text
    , id            :: Integer
    , memberCount   :: Integer
    , name          :: Text
    , private       :: Bool
    , profile       :: Text
    , profileMedium :: Text
    , resourceState :: Integer
    , sportType     :: Text
    , state         :: Text
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
