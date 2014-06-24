-- | Functions for performing actions against the API.
module Scurry.Actions
    ( getClub
    ) where

import           Scurry.Client  (Client)
import           Scurry.Objects (ClubDetailed)

-- | <http://strava.github.io/api/v3/clubs/#get-details>
getClub :: Client -> IO (Maybe ClubDetailed)
getClub _client = undefined
