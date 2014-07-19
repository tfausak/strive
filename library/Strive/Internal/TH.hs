-- | Helper functions for template Haskell, to avoid stage restrictions.
module Strive.Internal.TH
  ( options
  ) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Data.Char (isUpper, toLower)

-- | Default FromJSON options.
options :: Options
options = defaultOptions
  { fieldLabelModifier = underscore . dropPrefix
  }

underscore :: String -> String
underscore = concatMap go
 where
  go c = if isUpper c
    then ['_', toLower c]
    else [c]

dropPrefix :: String -> String
dropPrefix = tail . dropWhile (/= '_')
