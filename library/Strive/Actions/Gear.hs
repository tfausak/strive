-- | <http://strava.github.io/api/v3/gear/>
module Strive.Actions.Gear
    ( getGear
    ) where

import Data.Monoid ((<>))
import Strive.Client (Client)
import Strive.Client.HTTP (get)
import Strive.Objects (GearDetailed)
import Strive.Types (GearId)

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> GearId -> IO (Either String GearDetailed)
getGear client gearId = get client resource query
  where
    resource = "gear/" <> gearId
    query = [] :: [(String, String)]
