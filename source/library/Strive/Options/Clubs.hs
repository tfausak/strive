-- | 'Strive.Actions.Clubs'
module Strive.Options.Clubs
  ( GetClubMembersOptions
  , GetClubActivitiesOptions(..)
  ) where

import Data.Default (Default, def)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getClubMembers'
type GetClubMembersOptions = PaginationOptions

-- | 'Strive.Actions.getClubActivities'
data GetClubActivitiesOptions = GetClubActivitiesOptions
  { getClubActivitiesOptions_before :: Maybe UTCTime
  , getClubActivitiesOptions_after :: Maybe UTCTime
  , getClubActivitiesOptions_page :: Integer
  , getClubActivitiesOptions_perPage :: Integer
  }
  deriving Show

instance Default GetClubActivitiesOptions where
  def = GetClubActivitiesOptions
    { getClubActivitiesOptions_before = Nothing
    , getClubActivitiesOptions_after = Nothing
    , getClubActivitiesOptions_page = 1
    , getClubActivitiesOptions_perPage = 200
    }

instance QueryLike GetClubActivitiesOptions where
  toQuery options = toQuery
    [ ( "before"
      , fmap
        (show . utcTimeToPOSIXSeconds)
        (getClubActivitiesOptions_before options)
      )
    , ( "after"
      , fmap
        (show . utcTimeToPOSIXSeconds)
        (getClubActivitiesOptions_after options)
      )
    , ("page", Just (show (getClubActivitiesOptions_page options)))
    , ("per_page", Just (show (getClubActivitiesOptions_perPage options)))
    ]
