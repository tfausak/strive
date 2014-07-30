{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}


-- | Lens classes and instances generated with template Haskell.
module Strive.Internal.Lenses where

import Strive.Internal.TH (makeLenses)
import Strive.Lenses (Lens)

import Strive.Client
import Strive.Internal.Options
import Strive.Options
import Strive.Types

-- * Client

$(makeLenses "Client")

-- * Options

$(makeLenses "BuildAuthorizeUrlOptions")
$(makeLenses "CreateActivityOptions")
$(makeLenses "ExploreSegmentsOptions")
$(makeLenses "GetActivityCommentsOptions")
$(makeLenses "GetActivityOptions")
$(makeLenses "GetCurrentActivitiesOptions")
$(makeLenses "GetSegmentEffortsOptions")
$(makeLenses "GetSegmentLeaderboardOptions")
$(makeLenses "GetStreamsOptions")
$(makeLenses "PaginationOptions")
$(makeLenses "UpdateActivityOptions")
$(makeLenses "UpdateCurrentAthleteOptions")
$(makeLenses "UploadActivityOptions")

-- * Types

$(makeLenses "ActivityDetailed")
$(makeLenses "ActivityLapSummary")
$(makeLenses "ActivitySummary")
$(makeLenses "ActivityZoneDetailed")
$(makeLenses "ActivityZoneDistributionBucket")
$(makeLenses "AthleteDetailed")
$(makeLenses "AthleteMeta")
$(makeLenses "AthleteSummary")
$(makeLenses "ClubDetailed")
$(makeLenses "ClubSummary")
$(makeLenses "CommentSummary")
$(makeLenses "DeauthorizationResponse")
$(makeLenses "EffortDetailed")
$(makeLenses "GearDetailed")
$(makeLenses "GearSummary")
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
$(makeLenses "UploadStatus")
