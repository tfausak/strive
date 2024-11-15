-- | 'Strive.Actions.Athletes'
module Strive.Options.Athletes
  ( UpdateCurrentAthleteOptions (..),
    GetAthleteCrsOptions,
  )
where

import qualified Data.Monoid as Monoid
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (Gender)
import Strive.Internal.Options (PaginationOptions)

-- | 'Strive.Actions.updateCurrentAthlete'
data UpdateCurrentAthleteOptions = UpdateCurrentAthleteOptions
  { updateCurrentAthleteOptions_city :: Monoid.Last String,
    updateCurrentAthleteOptions_state :: Monoid.Last String,
    updateCurrentAthleteOptions_country :: Monoid.Last String,
    updateCurrentAthleteOptions_sex :: Monoid.Last Gender,
    updateCurrentAthleteOptions_weight :: Monoid.Last Double
  }
  deriving (Show)

instance Semigroup UpdateCurrentAthleteOptions where
  x <> y =
    UpdateCurrentAthleteOptions
      { updateCurrentAthleteOptions_city = updateCurrentAthleteOptions_city x <> updateCurrentAthleteOptions_city y,
        updateCurrentAthleteOptions_state = updateCurrentAthleteOptions_state x <> updateCurrentAthleteOptions_state y,
        updateCurrentAthleteOptions_country = updateCurrentAthleteOptions_country x <> updateCurrentAthleteOptions_country y,
        updateCurrentAthleteOptions_sex = updateCurrentAthleteOptions_sex x <> updateCurrentAthleteOptions_sex y,
        updateCurrentAthleteOptions_weight = updateCurrentAthleteOptions_weight x <> updateCurrentAthleteOptions_weight y
      }

instance Monoid UpdateCurrentAthleteOptions where
  mempty =
    UpdateCurrentAthleteOptions
      { updateCurrentAthleteOptions_city = mempty,
        updateCurrentAthleteOptions_state = mempty,
        updateCurrentAthleteOptions_country = mempty,
        updateCurrentAthleteOptions_sex = mempty,
        updateCurrentAthleteOptions_weight = mempty
      }

instance QueryLike UpdateCurrentAthleteOptions where
  toQuery options =
    toQuery
      [ ("city", Monoid.getLast (updateCurrentAthleteOptions_city options)),
        ("state", Monoid.getLast (updateCurrentAthleteOptions_state options)),
        ("country", Monoid.getLast (updateCurrentAthleteOptions_country options)),
        ("sex", fmap show (Monoid.getLast (updateCurrentAthleteOptions_sex options))),
        ("weight", fmap show (Monoid.getLast (updateCurrentAthleteOptions_weight options)))
      ]

-- | 'Strive.Actions.getAthleteCrs'
type GetAthleteCrsOptions = PaginationOptions
