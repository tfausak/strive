{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

-- | Lenses for easily getting and setting values.
module Strive.Lenses where

import Data.Functor.Constant (Constant (Constant), getConstant)
import Data.Functor.Identity (Identity (Identity), runIdentity)

import Strive.Client
import Strive.Internal.Options
import Strive.Internal.TH (makeLenses)
import Strive.Options
import Strive.Types

-- | A lens for a record.
type Lens a b = Functor f => (b -> f b) -> a -> f a

-- | Get a field from a record.
get :: Lens a b -> a -> b
get lens = getConstant . lens Constant

-- | Set a field in a record.
set :: Lens a b -> b -> a -> a
set lens x = update lens (const x)

-- | Update a field in a record
update :: Lens a b -> (b -> b) -> a -> a
update lens f = runIdentity . lens (Identity . f)

$(makeLenses "ActivityDetailed")
$(makeLenses "ActivityLapSummary")
$(makeLenses "ActivitySummary")
$(makeLenses "ActivityZoneDetailed")
$(makeLenses "ActivityZoneDistributionBucket")
$(makeLenses "AthleteDetailed")
$(makeLenses "AthleteMeta")
$(makeLenses "AthleteSummary")
$(makeLenses "BuildAuthorizeUrlOptions")
$(makeLenses "Client")
$(makeLenses "ClubDetailed")
$(makeLenses "ClubSummary")
$(makeLenses "CommentSummary")
$(makeLenses "CreateActivityOptions")
$(makeLenses "DeauthorizationResponse")
$(makeLenses "EffortDetailed")
$(makeLenses "ExploreSegmentsOptions")
$(makeLenses "GearDetailed")
$(makeLenses "GearSummary")
$(makeLenses "GetActivityCommentsOptions")
$(makeLenses "GetActivityOptions")
$(makeLenses "GetCurrentActivitiesOptions")
$(makeLenses "GetSegmentEffortsOptions")
$(makeLenses "GetSegmentLeaderboardOptions")
$(makeLenses "GetStreamsOptions")
$(makeLenses "PaginationOptions")
$(makeLenses "PhotoSummary")
$(makeLenses "PolylineDetailed")
$(makeLenses "PolylineSummary")
$(makeLenses "SegmentDetailed")
$(makeLenses "SegmentExplorerEntry")
$(makeLenses "SegmentExplorerResponse")
$(makeLenses "SegmentLeaderboardEntry")
$(makeLenses "SegmentLeaderboardResponse")
$(makeLenses "SegmentSummary")
$(makeLenses "StreamDetailed")
$(makeLenses "TokenExchangeResponse")
$(makeLenses "UpdateActivityOptions")
$(makeLenses "UpdateCurrentAthleteOptions")
$(makeLenses "UploadActivityOptions")
$(makeLenses "UploadStatus")
