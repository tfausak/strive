{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Strive.Lenses where

import Control.Lens.TH (makeFieldsWith, underscoreFields)
import Strive.Objects

$(makeFieldsWith underscoreFields ''ActivityDetailed)
$(makeFieldsWith underscoreFields ''ActivityLapSummary)
$(makeFieldsWith underscoreFields ''ActivitySummary)
$(makeFieldsWith underscoreFields ''ActivityZoneDetailed)
$(makeFieldsWith underscoreFields ''ActivityZoneDistributionBucket)
$(makeFieldsWith underscoreFields ''AthleteDetailed)
$(makeFieldsWith underscoreFields ''AthleteMeta)
$(makeFieldsWith underscoreFields ''AthleteSummary)
$(makeFieldsWith underscoreFields ''ClubDetailed)
$(makeFieldsWith underscoreFields ''ClubSummary)
$(makeFieldsWith underscoreFields ''CommentSummary)
$(makeFieldsWith underscoreFields ''DeauthorizationResponse)
$(makeFieldsWith underscoreFields ''EffortDetailed)
$(makeFieldsWith underscoreFields ''GearDetailed)
$(makeFieldsWith underscoreFields ''GearSummary)
$(makeFieldsWith underscoreFields ''PhotoSummary)
$(makeFieldsWith underscoreFields ''PolylineDetailed)
$(makeFieldsWith underscoreFields ''PolylineSummary)
$(makeFieldsWith underscoreFields ''SegmentDetailed)
$(makeFieldsWith underscoreFields ''SegmentExplorer)
$(makeFieldsWith underscoreFields ''SegmentExplorerEntry)
$(makeFieldsWith underscoreFields ''SegmentLeaderboard)
$(makeFieldsWith underscoreFields ''SegmentLeaderboardEntry)
$(makeFieldsWith underscoreFields ''SegmentSummary)
$(makeFieldsWith underscoreFields ''StreamDetailed)
$(makeFieldsWith underscoreFields ''TokenExchangeResponse)
$(makeFieldsWith underscoreFields ''UploadStatus)
