-- | <http://strava.github.io/api/v3/athlete/>
module Strive.Actions.Athletes
    ( getAthlete
    , getAthleteCRs
    , getCurrentAthlete
    ) where

import           Data.Monoid             ((<>))
import           Strive.Actions.Internal (get, paginate)
import           Strive.Client           (Client)
import           Strive.Objects          (AthleteDetailed, AthleteSummary,
                                          EffortSummary)
import           Strive.Types            (AthleteId, Page, PerPage)

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> AthleteId -> IO (Either String AthleteSummary)
getAthlete client athleteId = get client resource query
  where
    resource = "athletes/" <> show athleteId
    query = []

-- | <http://strava.github.io/api/v3/athlete/#koms>
getAthleteCRs :: Client -> AthleteId -> Page -> PerPage -> IO (Either String [EffortSummary])
getAthleteCRs client athleteId page perPage = get client resource query
  where
    resource = "athletes/" <> show athleteId <> "/koms"
    query = paginate page perPage

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String AthleteDetailed)
getCurrentAthlete client = get client resource query
  where
    resource = "athlete"
    query = []
