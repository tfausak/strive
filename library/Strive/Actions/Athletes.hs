{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/athlete/>
module Strive.Actions.Athletes
    ( getAthlete
    , getAthleteCRs
    , getCurrentAthlete
    , putCurrentAthlete
    ) where

import Data.ByteString.Char8 (pack, singleton)
import Data.Monoid ((<>))
import Strive.Client (Client)
import Strive.Client.HTTP (get, put)
import Strive.Objects (AthleteDetailed, AthleteSummary, EffortSummary)
import Strive.Types (AthleteId, Page, PerPage)
import Strive.Utilities (paginate, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> AthleteId -> IO (Either String AthleteSummary)
getAthlete client athleteId = get client resource query
  where
    resource = "athletes/" <> show athleteId
    query = [] :: [(String, String)]

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
    query = [] :: [(String, String)]

-- | <http://strava.github.io/api/v3/athlete/#update>
putCurrentAthlete :: Client -> Maybe String -> Maybe String -> Maybe String -> Maybe Char -> Maybe Double -> IO (Either String AthleteDetailed)
putCurrentAthlete client city state country sex weight = put client resource query
  where
    resource = "athlete"
    query = queryToSimpleQuery
        [ ("city", fmap pack city)
        , ("state", fmap pack state)
        , ("country", fmap pack country)
        , ("sex", fmap singleton sex)
        , ("weight", fmap (pack . show) weight)
        ]
