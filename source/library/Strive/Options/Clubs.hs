-- | 'Strive.Actions.Clubs'
module Strive.Options.Clubs
  ( GetClubMembersOptions,
    GetClubActivitiesOptions (..),
  )
where

import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getClubMembers'
type GetClubMembersOptions = PaginationOptions

-- | 'Strive.Actions.getClubActivities'
data GetClubActivitiesOptions = GetClubActivitiesOptions
  { getClubActivitiesOptions_before :: Monoid.Last UTCTime,
    getClubActivitiesOptions_after :: Monoid.Last UTCTime,
    getClubActivitiesOptions_page :: Semigroup.Last Integer,
    getClubActivitiesOptions_perPage :: Semigroup.Last Integer
  }
  deriving (Show)

instance Semigroup GetClubActivitiesOptions where
  x <> y =
    GetClubActivitiesOptions
      { getClubActivitiesOptions_before = getClubActivitiesOptions_before x <> getClubActivitiesOptions_before y,
        getClubActivitiesOptions_after = getClubActivitiesOptions_after x <> getClubActivitiesOptions_after y,
        getClubActivitiesOptions_page = getClubActivitiesOptions_page x <> getClubActivitiesOptions_page y,
        getClubActivitiesOptions_perPage = getClubActivitiesOptions_perPage x <> getClubActivitiesOptions_perPage y
      }

instance Monoid GetClubActivitiesOptions where
  mempty =
    GetClubActivitiesOptions
      { getClubActivitiesOptions_before = mempty,
        getClubActivitiesOptions_after = mempty,
        getClubActivitiesOptions_page = pure 1,
        getClubActivitiesOptions_perPage = pure 200
      }

instance QueryLike GetClubActivitiesOptions where
  toQuery options =
    toQuery
      [ ( "before",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (Monoid.getLast (getClubActivitiesOptions_before options))
        ),
        ( "after",
          fmap
            (show . utcTimeToPOSIXSeconds)
            (Monoid.getLast (getClubActivitiesOptions_after options))
        ),
        ("page", Just (show (Semigroup.getLast (getClubActivitiesOptions_page options)))),
        ("per_page", Just (show (Semigroup.getLast (getClubActivitiesOptions_perPage options))))
      ]
