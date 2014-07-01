{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Strive.Lenses where

import Control.Lens.TH (makeFields)
import Strive.Objects

$(makeFields ''ActivityDetailed)
$(makeFields ''ActivityLapSummary)
$(makeFields ''ActivitySummary)
$(makeFields ''ActivityZoneDetailed)
$(makeFields ''ActivityZoneDistributionBucket)
$(makeFields ''AthleteDetailed)
$(makeFields ''AthleteMeta)
$(makeFields ''AthleteSummary)
$(makeFields ''ClubDetailed)
$(makeFields ''ClubSummary)
$(makeFields ''CommentSummary)
$(makeFields ''DeauthorizationResponse)
$(makeFields ''EffortDetailed)
$(makeFields ''GearDetailed)
$(makeFields ''GearSummary)
$(makeFields ''PhotoSummary)
$(makeFields ''PolylineDetailed)
$(makeFields ''PolylineSummary)
$(makeFields ''SegmentDetailed)
$(makeFields ''SegmentExplorer)
$(makeFields ''SegmentExplorerEntry)
$(makeFields ''SegmentLeaderboard)
$(makeFields ''SegmentLeaderboardEntry)
$(makeFields ''SegmentSummary)
$(makeFields ''StreamDetailed)
$(makeFields ''TokenExchangeResponse)
$(makeFields ''UploadStatus)
