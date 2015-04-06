-- | Aliases for common types.
module Strive.Aliases where

import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Network.HTTP.Conduit (Response)

type ActivityId = Integer
type ApplicationId = Integer
type ApplicationSecret = String
type AthleteId = Integer
type AuthorizationCode = String
type ClubId = Integer
type EffortId = Integer
type ElapsedTime = Integer
type Extension = String
type GearId = String
type Latitude = Double
type Longitude = Double
type Name = String
type RedirectUri = String
type Result a = Either (Response ByteString, String) a
type SegmentId = Integer
type StartTime = UTCTime
type StreamId = Integer
type UploadId = Integer
