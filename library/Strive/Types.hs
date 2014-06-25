-- | Type aliases for better function signatures.
module Strive.Types
    ( ActivityId
    , AthleteId
    , ClubId
    , EffortId
    , GearId
    , IncludeMarkdown
    , Page
    , PerPage
    , Resource
    , SegmentId
    ) where

-- | Activity ID
type ActivityId = Integer

-- | Athlete ID
type AthleteId = Integer

-- | Club ID
type ClubId = Integer

-- | Effort ID
type EffortId = Integer

-- | Gear ID
type GearId = String

-- | Do you want to include Markdown?
type IncludeMarkdown = Bool

-- | Page number
type Page = Maybe Integer

-- | Number of elements per page
type PerPage = Maybe Integer

-- | Resource path
type Resource = String

-- | Segment ID
type SegmentId = Integer
