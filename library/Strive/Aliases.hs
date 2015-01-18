-- | Aliases for common types.
module Strive.Aliases
  ( Result
  ) where

-- | An action's result.
type Result a = IO (Either String a)
