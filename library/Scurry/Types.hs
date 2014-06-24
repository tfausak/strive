-- | Type aliases for better function signatures.
module Scurry.Types
    ( ActivityId
    , AthleteId
    , ClubId
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

-- | Gear ID
type GearId = String

-- | Do you want to include Markdown?
type IncludeMarkdown = Bool

-- | Page number
type Page = Integer

-- | Number of elements per page
type PerPage = Integer

-- | Resource path
type Resource = String

-- | Segment ID
type SegmentId = Integer
