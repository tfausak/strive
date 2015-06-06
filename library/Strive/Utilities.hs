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

altitudeStream :: [StreamDetailed] -> Maybe [Double]
altitudeStream = lookupStream AltitudeStream

cadenceStream :: [StreamDetailed] -> Maybe [Integer]
cadenceStream = lookupStream CadenceStream

distanceStream :: [StreamDetailed] -> Maybe [Double]
distanceStream = lookupStream DistanceStream

gradeSmoothStream :: [StreamDetailed] -> Maybe [Double]
gradeSmoothStream = lookupStream GradeSmoothStream

heartrateStream :: [StreamDetailed] -> Maybe [Integer]
heartrateStream = lookupStream HeartrateStream

latlngStream :: [StreamDetailed] -> Maybe [(Double, Double)]
latlngStream = lookupStream LatlngStream

movingStream :: [StreamDetailed] -> Maybe [Bool]
movingStream = lookupStream MovingStream

tempStream :: [StreamDetailed] -> Maybe [Integer]
tempStream = lookupStream TempStream

timeStream :: [StreamDetailed] -> Maybe [Integer]
timeStream = lookupStream TimeStream

velocitySmoothStream :: [StreamDetailed] -> Maybe [Double]
velocitySmoothStream = lookupStream VelocitySmoothStream

wattsStream :: [StreamDetailed] -> Maybe [Integer]
wattsStream = lookupStream WattsStream

lookupStream :: FromJSON a => StreamType -> [StreamDetailed] -> Maybe [a]
lookupStream stream_type streams =
  let resultToMaybe (Success a) = Just a
      resultToMaybe (Error _)   = Nothing
  in do stream <- find (\stream ->
            (streamDetailed_type stream) == (pack $ show stream_type)) streams
        mapM (resultToMaybe . fromJSON) (streamDetailed_data stream)
