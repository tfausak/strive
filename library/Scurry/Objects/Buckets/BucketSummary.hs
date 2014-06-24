{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/activities/#zones>
module Scurry.Objects.Buckets.BucketSummary
    ( BucketSummary (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))

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
