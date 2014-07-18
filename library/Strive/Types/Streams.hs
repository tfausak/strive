{-# LANGUAGE OverloadedStrings #-}

module Strive.Types.Streams
  ( StreamDetailed (..)
  ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Strive.Enums (Resolution, SeriesType)

-- | <http://strava.github.io/api/v3/streams/#detailed>
data StreamDetailed = StreamDetailed
  { streamDetailed_data         :: [Value]
  , streamDetailed_originalSize :: Integer
  , streamDetailed_resolution   :: Resolution
  , streamDetailed_seriesType   :: SeriesType
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
