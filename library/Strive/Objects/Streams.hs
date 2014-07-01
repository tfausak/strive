{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/streams/>
module Strive.Objects.Streams
    ( StreamDetailed (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)

-- | <http://strava.github.io/api/v3/streams/#detailed>
data StreamDetailed = StreamDetailed
    { streamDetailed_data         :: [Value]
    , streamDetailed_originalSize :: Integer
    , streamDetailed_resolution   :: Text
    , streamDetailed_seriesType   :: Text
    , streamDetailed_type         :: Text
    } deriving Show

instance FromJSON StreamDetailed where
    parseJSON (Object o) = StreamDetailed
        <$> o .: "data"
        <*> o .: "original_size"
        <*> o .: "resolution"
        <*> o .: "series_type"
        <*> o .: "type"
    parseJSON _ = empty
