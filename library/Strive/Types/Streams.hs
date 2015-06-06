{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/streams/>
module Strive.Types.Streams
  ( StreamDetailed (..)
  , altitudeStream
  , cadenceStream
  , distanceStream
  , gradeSmoothStream
  , heartrateStream
  , latlngStream
  , movingStream
  , tempStream
  , timeStream
  , velocitySmoothStream
  , wattsStream
  ) where

import Data.Aeson (Value, Result(..), FromJSON(..), fromJSON)
import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text, pack)
import Strive.Enums (Resolution, SeriesType, StreamType(..))
import Strive.Internal.TH (options)


import Data.List (find)

-- | <http://strava.github.io/api/v3/streams/#detailed>
data StreamDetailed = StreamDetailed
  { streamDetailed_data         :: [Value]
  , streamDetailed_originalSize :: Integer
  , streamDetailed_resolution   :: Resolution
  , streamDetailed_seriesType   :: SeriesType
  , streamDetailed_type         :: Text
  } deriving Show

$(deriveFromJSON options ''StreamDetailed)

lookupStream :: FromJSON a => StreamType -> [StreamDetailed] -> Maybe [a]
lookupStream stream_type streams = 
  let resultToMaybe (Success a) = Just a
      resultToMaybe (Error _)   = Nothing
  in do stream <- find (\stream ->
            (streamDetailed_type stream) == (pack $ show stream_type)) streams
        mapM (resultToMaybe . fromJSON) (streamDetailed_data stream)

altitudeStream :: [StreamDetailed] -> Maybe [Int]
altitudeStream = lookupStream AltitudeStream

cadenceStream :: [StreamDetailed] -> Maybe [Int]
cadenceStream = lookupStream CadenceStream

distanceStream :: [StreamDetailed] -> Maybe [Float]
distanceStream = lookupStream DistanceStream

gradeSmoothStream :: [StreamDetailed] -> Maybe [Float]
gradeSmoothStream = lookupStream GradeSmoothStream

heartrateStream :: [StreamDetailed] -> Maybe [Int]
heartrateStream = lookupStream HeartrateStream

latlngStream :: [StreamDetailed] -> Maybe [(Float, Float)]
latlngStream = lookupStream LatlngStream

movingStream :: [StreamDetailed] -> Maybe [Bool]
movingStream = lookupStream MovingStream

tempStream :: [StreamDetailed] -> Maybe [Int]
tempStream = lookupStream TempStream

timeStream :: [StreamDetailed] -> Maybe [Int]
timeStream = lookupStream TimeStream

velocitySmoothStream :: [StreamDetailed] -> Maybe [Float]
velocitySmoothStream = lookupStream VelocitySmoothStream

wattsStream :: [StreamDetailed] -> Maybe [Int]
wattsStream = lookupStream WattsStream


