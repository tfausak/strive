{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#zones>
module Strive.Objects.Buckets.BucketSummary
    ( BucketSummary (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))

-- | Summary representation of a distribution bucket.
data BucketSummary = BucketSummary
    { max  :: Integer
    , min  :: Integer
    , time :: Integer
    } deriving Show

instance FromJSON BucketSummary where
    parseJSON (Object o) = BucketSummary
        <$> o .: "max"
        <*> o .: "min"
        <*> o .: "time"
    parseJSON _ = empty
