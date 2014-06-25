{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/clubs/#summary>
module Strive.Objects.Clubs.ClubSummary
    ( ClubSummary (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)

-- | Summary representation of a club.
data ClubSummary = ClubSummary
    { id            :: Integer
    , name          :: Text
    , profile       :: Text
    , profileMedium :: Text
    , resourceState :: Integer
    } deriving Show

instance FromJSON ClubSummary where
    parseJSON (Object o) = ClubSummary
        <$> o .: "id"
        <*> o .: "name"
        <*> o .: "profile"
        <*> o .: "profile_medium"
        <*> o .: "resource_state"
    parseJSON _ = empty
