-- | <http://strava.github.io/api/v3/athlete/>
module Strive.Actions.Athletes
  ( getCurrentAthlete
  , getAthlete
  , updateCurrentAthlete
  , getAthleteCrs
  ) where

import Network.HTTP.Types (Query, toQuery)
import Strive.Aliases (Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get, put)
import Strive.Options (GetAthleteCrsOptions, UpdateCurrentAthleteOptions)
import Strive.Types (AthleteDetailed, AthleteSummary, EffortDetailed)

-- TODO: Move to Strive.Aliases.
type AthleteId = Integer

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> Result AthleteDetailed
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> AthleteId -> Result AthleteSummary
getAthlete client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" ++ show athleteId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#update>
updateCurrentAthlete :: Client -> UpdateCurrentAthleteOptions -> Result AthleteDetailed
updateCurrentAthlete client options = put client resource query
 where
  resource = "api/v3/athlete"
  query = toQuery options

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCrs :: Client -> AthleteId -> GetAthleteCrsOptions -> Result [EffortDetailed]
getAthleteCrs client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" ++ show athleteId ++ "/koms"
  query = toQuery options
