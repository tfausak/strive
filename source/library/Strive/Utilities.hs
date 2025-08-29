-- | Utility functions for making common actions easier.
module Strive.Utilities
  ( -- * Streams
    altitudeStream,
    cadenceStream,
    distanceStream,
    gradeSmoothStream,
    heartrateStream,
    latlngStream,
    movingStream,
    tempStream,
    timeStream,
    velocitySmoothStream,
    wattsStream,
  )
where

import Data.Aeson (FromJSON, Result (Error, Success), Value, fromJSON)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import qualified Strive.Enums as Enums
import Strive.Types (StreamDetailed, streamDetailed_data, streamDetailed_type)

altitudeStream :: StreamDetailed -> Maybe [Double]
altitudeStream = lookupStream Enums.AltitudeStream

cadenceStream :: StreamDetailed -> Maybe [Integer]
cadenceStream = lookupStream Enums.CadenceStream

distanceStream :: StreamDetailed -> Maybe [Double]
distanceStream = lookupStream Enums.DistanceStream

gradeSmoothStream :: StreamDetailed -> Maybe [Double]
gradeSmoothStream = lookupStream Enums.GradeSmoothStream

heartrateStream :: StreamDetailed -> Maybe [Integer]
heartrateStream = lookupStream Enums.HeartrateStream

latlngStream :: StreamDetailed -> Maybe [(Double, Double)]
latlngStream = lookupStream Enums.LatlngStream

movingStream :: StreamDetailed -> Maybe [Bool]
movingStream = lookupStream Enums.MovingStream

tempStream :: StreamDetailed -> Maybe [Integer]
tempStream = lookupStream Enums.TempStream

timeStream :: StreamDetailed -> Maybe [Integer]
timeStream = lookupStream Enums.TimeStream

velocitySmoothStream :: StreamDetailed -> Maybe [Double]
velocitySmoothStream = lookupStream Enums.VelocitySmoothStream

wattsStream :: StreamDetailed -> Maybe [Integer]
wattsStream = lookupStream Enums.WattsStream

lookupStream :: (FromJSON a) => Enums.StreamType -> StreamDetailed -> Maybe [a]
lookupStream streamType stream =
  if streamDetailed_type stream == pack (show streamType)
    then Just (mapMaybe maybeFromJson (streamDetailed_data stream))
    else Nothing

maybeFromJson :: (FromJSON a) => Value -> Maybe a
maybeFromJson value = case fromJSON value of
  Success x -> Just x
  Error _ -> Nothing
