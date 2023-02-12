{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/streams/>
module Strive.Types.Streams
  ( StreamDetailed (..),
  )
where

import Data.Aeson (Value)
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Enums (Resolution, SeriesType)
import Strive.Internal.TH (options)

-- | <http://strava.github.io/api/v3/streams/#detailed>
data StreamDetailed = StreamDetailed
  { streamDetailed_data :: [Value],
    streamDetailed_originalSize :: Integer,
    streamDetailed_resolution :: Resolution,
    streamDetailed_seriesType :: SeriesType,
    streamDetailed_type :: Text
  }
  deriving (Show)

$(deriveFromJSON options ''StreamDetailed)
