module Strive.Actions.Athletes
  ( getCurrentAthlete
  , getAthlete
  , updateCurrentAthlete
  , getAthleteCrs
  ) where

import Network.HTTP.Types (Query, toQuery)
import Strive.Client (Client)
import Strive.Internal.HTTP (get, put)
import Strive.Options (GetAthleteCrsOptions, UpdateCurrentAthleteOptions)
import Strive.Types (AthleteDetailed, AthleteSummary, EffortDetailed)

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String AthleteDetailed)
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Integer -> IO (Either String AthleteSummary)
getAthlete client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" ++ show athleteId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#update>
updateCurrentAthlete :: Client -> UpdateCurrentAthleteOptions -> IO (Either String AthleteDetailed)
updateCurrentAthlete client options = put client resource query
 where
  resource = "api/v3/athlete"
  query = toQuery options

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCrs :: Client -> Integer -> GetAthleteCrsOptions -> IO (Either String [EffortDetailed])
getAthleteCrs client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" ++ show athleteId ++ "/koms"
  query = toQuery options
