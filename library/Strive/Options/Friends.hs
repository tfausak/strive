module Strive.Options.Friends
  ( GetCurrentFriendsOptions (..)
  , GetFriendsOptions (..)
  , GetCurrentFollowersOptions (..)
  , GetFollowersOptions (..)
  , GetCommonFriendsOptions (..)
  ) where

import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getCurrentFriends'
type GetCurrentFriendsOptions = PaginationOptions

-- | 'Strive.Actions.getFriends'
type GetFriendsOptions = PaginationOptions

-- | 'Strive.Actions.getCurrentFollowers'
type GetCurrentFollowersOptions = PaginationOptions

-- | 'Strive.Actions.getFollowers'
type GetFollowersOptions = PaginationOptions

-- | 'Strive.Actions.getCommonFriends'
type GetCommonFriendsOptions = PaginationOptions
