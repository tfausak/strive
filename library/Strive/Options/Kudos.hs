-- | <http://strava.github.io/api/v3/kudos/>
module Strive.Options.Kudos
  ( GetActivityKudoersOptions (..)
  ) where

import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.getActivityKudoers'
type GetActivityKudoersOptions = PaginationOptions
