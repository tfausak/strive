-- | <http://strava.github.io/api/v3/athlete/>
module Strive.Actions.Athletes
  ( getCurrentAthlete
  , getAthlete
  , updateCurrentAthlete
  , getAthleteStats
  , getAthleteCrs
  ) where

import Network.HTTP.Types (Query, toQuery)
import Strive.Aliases (AthleteId, Result)
import Strive.Client (Client)
import Strive.Internal.HTTP (get, put)
import Strive.Options (GetAthleteCrsOptions, UpdateCurrentAthleteOptions)
import Strive.Types
  (AthleteDetailed, AthleteStats, AthleteSummary, EffortDetailed)

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Result AthleteDetailed)
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> AthleteId -> IO (Result AthleteSummary)
getAthlete client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#update>
updateCurrentAthlete
  :: Client -> UpdateCurrentAthleteOptions -> IO (Result AthleteDetailed)
updateCurrentAthlete client options = put client resource query
 where
  resource = "api/v3/athlete"
  query = toQuery options

-- | <http://strava.github.io/api/v3/athlete/#stats>
getAthleteStats :: Client -> Integer -> IO (Result AthleteStats)
getAthleteStats client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/stats"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCrs
  :: Client
  -> AthleteId
  -> GetAthleteCrsOptions
  -> IO (Result [EffortDetailed])
getAthleteCrs client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/koms"
  query = toQuery options
