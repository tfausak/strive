{-# LANGUAGE OverloadedStrings #-}

-- | Functions for performing actions against the API.
module Strive.Actions
    ( getPhotos
    , module Actions
    ) where

import           Data.Monoid               ((<>))
import           Strive.Actions.Activities as Actions
import           Strive.Actions.Athletes   as Actions
import           Strive.Actions.Clubs      as Actions
import           Strive.Actions.Comments   as Actions
import           Strive.Actions.Efforts    as Actions
import           Strive.Actions.Friends    as Actions
import           Strive.Actions.Gear       as Actions
import           Strive.Actions.Internal   (get)
import           Strive.Actions.Kudos      as Actions
import           Strive.Actions.Segments   as Actions
import           Strive.Client             (Client)
import qualified Strive.Objects            as Objects
import qualified Strive.Types              as Types

-- | <http://strava.github.io/api/v3/photos/#list>
getPhotos :: Client -> Types.ActivityId -> IO (Either String [Objects.PhotoSummary])
getPhotos client activityId = get client resource query
  where
    resource = "activities/" <> show activityId <> "/photos"
    query = []
