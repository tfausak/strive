module Strive.Actions.Gear
  ( getGear
  ) where

import Data.Monoid ((<>))
import Network.HTTP.Types (Query)
import Strive.Client (Client)
import Strive.Internal.HTTP (get)
import Strive.Types (GearDetailed)

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> String -> IO (Either String GearDetailed)
getGear client gearId = get client resource query
 where
  resource = "api/v3/gear/" <> gearId
  query = [] :: Query
