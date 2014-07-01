{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/comments/>
module Strive.Objects.Comments
    ( CommentSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Objects.Athletes (AthleteSummary)

-- | <http://strava.github.io/api/v3/comments/#summary-and-detailed-representation-attributes>
data CommentSummary = CommentSummary
    { commentSummaryActivityId    :: Integer
    , commentSummaryAthlete       :: AthleteSummary
    , commentSummaryCreatedAt     :: UTCTime
    , commentSummaryId            :: Integer
    , commentSummaryResourceState :: Integer
    , commentSummaryText          :: Text
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
