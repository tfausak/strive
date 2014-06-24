{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/comments/#summary-and-detailed-representation-attributes>
module Scurry.Objects.Comments.CommentSummary
    ( CommentSummary (..)
    ) where

import           Control.Applicative     (empty, (<$>), (<*>))
import           Data.Aeson              (FromJSON, Value (Object), parseJSON,
                                          (.:))
import           Data.Text               (Text)
import           Data.Time.Clock         (UTCTime)
import           Scurry.Objects.Athletes (AthleteSummary)

-- | Summary representation of a comment.
data CommentSummary = CommentSummary
    { activityId    :: Integer
    , athlete       :: AthleteSummary
    , createdAt     :: UTCTime
    , id            :: Integer
    , resourceState :: Integer
    , text          :: Text
    } deriving Show

instance FromJSON CommentSummary where
    parseJSON (Object o) = CommentSummary
        <$> o .: "activity_id"
        <*> o .: "athlete"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .: "resource_state"
        <*> o .: "text"
    parseJSON _ = empty
