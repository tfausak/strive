{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/athlete/#summary>
module Strive.Objects.Athletes.AthleteSummary
    ( AthleteSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:), (.:?))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | Summary representation of an athlete.
data AthleteSummary = AthleteSummary
    { city          :: Maybe Text
    , country       :: Maybe Text
    , createdAt     :: UTCTime
    , firstname     :: Text
    , follower      :: Maybe Text
    , friend        :: Maybe Text
    , id            :: Integer
    , lastname      :: Text
    , premium       :: Bool
    , profile       :: Text
    , profileMedium :: Text
    , resourceState :: Integer
    , sex           :: Maybe Char
    , state         :: Text
    , updatedAt     :: UTCTime
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
