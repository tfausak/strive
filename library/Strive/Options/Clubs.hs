-- | 'Strive.Actions.Clubs'
module Strive.Options.Clubs
  ( GetClubMembersOptions
  , GetClubActivitiesOptions
  ) where

import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getClubMembers'
type GetClubMembersOptions = PaginationOptions

-- | 'Strive.Actions.getClubActivities'
type GetClubActivitiesOptions = PaginationOptions
