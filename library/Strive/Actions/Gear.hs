module Strive.Actions.Gear
  ( getGear
  ) where

-- | <http://strava.github.io/api/v3/gear/#show>
getGear :: Client -> String -> IO (Either String T.GearDetailed)
getGear client gearId = get client resource query
 where
  resource = "api/v3/gear/" <> gearId
  query = [] :: Query
