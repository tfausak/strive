{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/athlete/#detailed>
module Strive.Objects.Athletes.AthleteDetailed
    ( AthleteDetailed (..)
    ) where

import           Control.Applicative  (empty, (<$>), (<*>))
import           Data.Aeson           (FromJSON, Value (Object), parseJSON,
                                       (.:), (.:?))
import           Data.Text            (Text)
import           Data.Time.Clock      (UTCTime)
import           Strive.Objects.Clubs (ClubSummary)
import           Strive.Objects.Gear  (GearSummary)

-- | Detailed representation of an athlete.
data AthleteDetailed = AthleteDetailed
    { bikes                 :: [GearSummary]
    , city                  :: Text
    , clubs                 :: [ClubSummary]
    , country               :: Text
    , createdAt             :: UTCTime
    , datePreference        :: Text
    , email                 :: Text
    , firstname             :: Text
    , follower              :: Maybe Text
    , followerCount         :: Integer
    , friend                :: Maybe Text
    , friendCount           :: Integer
    , ftp                   :: Maybe Integer
    , id                    :: Integer
    , lastname              :: Text
    , measurementPreference :: Text
    , mutualFriendCount     :: Integer
    , premium               :: Bool
    , profile               :: Text
    , profileMedium         :: Text
    , resourceState         :: Integer
    , sex                   :: Maybe Char
    , shoes                 :: [GearSummary]
    , state                 :: Text
    , updatedAt             :: UTCTime
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
