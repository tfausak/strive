-- | Utility functions for making common actions easier.
module Strive.Utilities
  ( altitudeStream
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
  , with
  ) where

import Data.Aeson (Result (..), FromJSON (..), fromJSON)
import Data.Default (Default, def)
import Data.List (find)
import Data.Text (pack)
import Strive.Enums (StreamType (..))
import Strive.Types (StreamDetailed (..))

-- | Modify an action's default options by listing changes to it.
with :: Default a => [a -> a] -> a
with = foldr ($) def

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

lookupStream :: FromJSON a => StreamType -> [StreamDetailed] -> Maybe [a]
lookupStream stream_type streams =
  let resultToMaybe (Success a) = Just a
      resultToMaybe (Error _)   = Nothing
  in do stream <- find (\stream ->
            (streamDetailed_type stream) == (pack $ show stream_type)) streams
        mapM (resultToMaybe . fromJSON) (streamDetailed_data stream)
