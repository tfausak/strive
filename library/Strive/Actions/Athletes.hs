module Strive.Actions.Athletes
  ( getCurrentAthlete
  , getAthlete
  , updateCurrentAthlete
  , getAthleteCrs
  ) where

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String T.AthleteDetailed)
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Integer -> IO (Either String T.AthleteSummary)
getAthlete client athleteId = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#update>
updateCurrentAthlete :: Client -> O.UpdateCurrentAthleteOptions -> IO (Either String T.AthleteDetailed)
updateCurrentAthlete client options = put client resource query
 where
  resource = "api/v3/athlete"
  query = toQuery options

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCrs :: Client -> Integer -> O.GetAthleteCrsOptions -> IO (Either String [T.EffortDetailed])
getAthleteCrs client athleteId options = get client resource query
 where
  resource = "api/v3/athletes/" <> show athleteId <> "/koms"
  query = toQuery options
