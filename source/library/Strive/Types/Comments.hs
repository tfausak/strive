{-# LANGUAGE TemplateHaskell #-}

-- | <http://strava.github.io/api/v3/comments/>
module Strive.Types.Comments
  ( CommentSummary (..),
  )
where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Strive.Enums (ResourceState)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteSummary)

-- | <http://strava.github.io/api/v3/comments/#summary-and-detailed-representation-attributes>
data CommentSummary = CommentSummary
  { commentSummary_activityId :: Integer,
    commentSummary_athlete :: AthleteSummary,
    commentSummary_createdAt :: UTCTime,
    commentSummary_id :: Integer,
    commentSummary_resourceState :: ResourceState,
    commentSummary_text :: Text
  }
  deriving (Show)

$(deriveFromJSON options ''CommentSummary)
