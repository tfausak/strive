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

import Data.Aeson (FromJSON, Result (Error, Success), fromJSON)
import Data.Default (Default, def)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Strive.Enums (StreamType (..))
import Strive.Types (StreamDetailed (..))

-- | Modify an action's default options by listing changes to it.
with :: Default a => [a -> a] -> a
with = foldr ($) def

altitudeStream :: StreamDetailed -> Maybe [Double]
altitudeStream = lookupStream AltitudeStream

cadenceStream :: StreamDetailed -> Maybe [Integer]
cadenceStream = lookupStream CadenceStream

distanceStream :: StreamDetailed -> Maybe [Double]
distanceStream = lookupStream DistanceStream

gradeSmoothStream :: StreamDetailed -> Maybe [Double]
gradeSmoothStream = lookupStream GradeSmoothStream

heartrateStream :: StreamDetailed -> Maybe [Integer]
heartrateStream = lookupStream HeartrateStream

latlngStream :: StreamDetailed -> Maybe [(Double, Double)]
latlngStream = lookupStream LatlngStream

movingStream :: StreamDetailed -> Maybe [Bool]
movingStream = lookupStream MovingStream

tempStream :: StreamDetailed -> Maybe [Integer]
tempStream = lookupStream TempStream

timeStream :: StreamDetailed -> Maybe [Integer]
timeStream = lookupStream TimeStream

velocitySmoothStream :: StreamDetailed -> Maybe [Double]
velocitySmoothStream = lookupStream VelocitySmoothStream

wattsStream :: StreamDetailed -> Maybe [Integer]
wattsStream = lookupStream WattsStream

lookupStream :: FromJSON a => StreamType -> StreamDetailed -> Maybe [a]
lookupStream streamType stream =
  if streamDetailed_type stream == pack (show streamType)
    then Just (mapMaybe f (streamDetailed_data stream))
    else Nothing
 where
  f value = case fromJSON value of
    Success x -> Just x
    Error _ -> Nothing
