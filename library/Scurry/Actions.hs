-- | Functions for performing actions against the API.
module Scurry.Actions
    ( getClub
    ) where

import           Data.Monoid             ((<>))
import           Scurry.Actions.Internal (get)
import           Scurry.Client           (Client)
import           Scurry.Objects          (ClubDetailed)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> Integer -> IO (Maybe ClubDetailed)
getClub client clubId = get client resource query
  where
    resource = "clubs/" <> show clubId
    query = []
