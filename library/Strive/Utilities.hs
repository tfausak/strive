-- | Utility functions for making common actions easier.
module Strive.Utilities
  ( with
  ) where

import Data.Default (Default, def)

-- | Modify an action's default options by listing changes to it.
with :: Default a => [a -> a] -> a
with = foldr ($) def
