-- | <http://strava.github.io/api/v3/gear/>
module Strive.Actions.Gear
  ( getGear
  ) where

import Network.HTTP.Types (Query)
import Strive.Aliases (Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Types (GearDetailed)

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> String -> Result GearDetailed
getGear client gearId = get client resource query
 where
  resource = "api/v3/gear/" ++ gearId
  query = [] :: Query
