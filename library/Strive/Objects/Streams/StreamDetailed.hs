{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/streams/#detailed>
module Strive.Objects.Streams.StreamDetailed
    ( StreamDetailed (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))
import           Data.Text           (Text)

-- | Detailed representation of a stream.
data StreamDetailed = StreamDetailed
    { data_        :: [Value]
    , originalSize :: Integer
    , resolution   :: Text
    , seriesType   :: Text
    , type_        :: Text
    } deriving Show

instance FromJSON StreamDetailed where
    parseJSON (Object o) = StreamDetailed
        <$> o .: "data"
        <*> o .: "original_size"
        <*> o .: "resolution"
        <*> o .: "series_type"
        <*> o .: "type"
    parseJSON _ = empty
