{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/photos/>
module Strive.Types.Photos
  ( PhotoSummary(..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (PhotoType, ResourceState)
import Strive.Internal.TH (options)

-- | <http://strava.github.io/api/v3/photos/#summary-and-detailed-representation-attributes>
data PhotoSummary = PhotoSummary
  { photoSummary_activityId :: Integer
  , photoSummary_caption :: Text
  , photoSummary_createdAt :: UTCTime
  , photoSummary_id :: Integer
  , photoSummary_location :: Maybe (Double, Double)
  , photoSummary_ref :: Text
  , photoSummary_resourceState :: ResourceState
  , photoSummary_type :: PhotoType
  , photoSummary_uid :: Text
  , photoSummary_uploadedAt :: UTCTime
  }
  deriving Show

$(deriveFromJSON options ''PhotoSummary)
