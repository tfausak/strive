{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/athlete/#meta>
module Scurry.Objects.Athletes.AthleteMeta
    ( AthleteMeta (..)
    ) where

import           Control.Applicative (empty, (<$>), (<*>))
import           Data.Aeson          (FromJSON, Value (Object), parseJSON,
                                      (.:))

-- | Meta representation of an athlete.
data AthleteMeta = AthleteMeta
    { id            :: Integer
    , resourceState :: Integer
    } deriving Show

instance FromJSON AthleteMeta where
    parseJSON (Object o) = AthleteMeta
        <$> o .: "id"
        <*> o .: "resource_state"
    parseJSON _ = empty
