-- | Functions for performing actions against the API.
module Strive.Actions
  ( module Strive.Actions.Activities
  , module Strive.Actions.Athletes
  , module Strive.Actions.Authentication
  , module Strive.Actions.Clubs
  , module Strive.Actions.Comments
  , module Strive.Actions.Efforts
  , module Strive.Actions.Friends
  , module Strive.Actions.Gear
  , module Strive.Actions.Kudos
  , module Strive.Actions.Photos
  , module Strive.Actions.Segments
  , module Strive.Actions.Streams
  , module Strive.Actions.Uploads
  , with
  , (?)
  ) where

import Strive.Actions.Activities
import Strive.Actions.Athletes
import Strive.Actions.Authentication
import Strive.Actions.Clubs
import Strive.Actions.Comments
import Strive.Actions.Efforts
import Strive.Actions.Friends
import Strive.Actions.Gear
import Strive.Actions.Kudos
import Strive.Actions.Photos
import Strive.Actions.Segments
import Strive.Actions.Streams
import Strive.Actions.Uploads

import Data.Default (Default, def)

-- | Helper function for easily performing actions.
with :: Default a => [a -> a] -> a
with = foldr ($) def

-- | Infix alias of '$ with'.
infixr 0 ?
(?) :: Default a => (a -> b) -> [a -> a] -> b
(?) = (. with)
