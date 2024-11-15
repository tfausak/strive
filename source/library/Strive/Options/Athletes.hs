-- | 'Strive.Actions.Athletes'
module Strive.Options.Athletes
  ( UpdateCurrentAthleteOptions (..),
    defaultUpdateCurrentAthleteOptions,
    GetAthleteCrsOptions,
  )
where

import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (Gender)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.updateCurrentAthlete'
data UpdateCurrentAthleteOptions = UpdateCurrentAthleteOptions
  { updateCurrentAthleteOptions_city :: Maybe String,
    updateCurrentAthleteOptions_state :: Maybe String,
    updateCurrentAthleteOptions_country :: Maybe String,
    updateCurrentAthleteOptions_sex :: Maybe Gender,
    updateCurrentAthleteOptions_weight :: Maybe Double
  }
  deriving (Show)

defaultUpdateCurrentAthleteOptions :: UpdateCurrentAthleteOptions
defaultUpdateCurrentAthleteOptions =
  UpdateCurrentAthleteOptions
    { updateCurrentAthleteOptions_city = Nothing,
      updateCurrentAthleteOptions_state = Nothing,
      updateCurrentAthleteOptions_country = Nothing,
      updateCurrentAthleteOptions_sex = Nothing,
      updateCurrentAthleteOptions_weight = Nothing
    }

instance QueryLike UpdateCurrentAthleteOptions where
  toQuery options =
    toQuery
      [ ("city", updateCurrentAthleteOptions_city options),
        ("state", updateCurrentAthleteOptions_state options),
        ("country", updateCurrentAthleteOptions_country options),
        ("sex", fmap show (updateCurrentAthleteOptions_sex options)),
        ("weight", fmap show (updateCurrentAthleteOptions_weight options))
      ]

-- | 'Strive.Actions.getAthleteCrs'
type GetAthleteCrsOptions = PaginationOptions
