-- | Common options that apply to many endpoints.
module Strive.Internal.Options
  ( PaginationOptions (..),
  )
where

import qualified Data.Semigroup as Semigroup
import Network.HTTP.Types (QueryLike, toQuery)

-- | Options for paginating.
data PaginationOptions = PaginationOptions
  { paginationOptions_page :: Semigroup.Last Integer,
    paginationOptions_perPage :: Semigroup.Last Integer
  }
  deriving (Show)

instance Semigroup PaginationOptions where
  x <> y =
    PaginationOptions
      { paginationOptions_page = paginationOptions_page x <> paginationOptions_page y,
        paginationOptions_perPage = paginationOptions_perPage x <> paginationOptions_perPage y
      }

instance Monoid PaginationOptions where
  mempty =
    PaginationOptions
      { paginationOptions_page = pure 1,
        paginationOptions_perPage = pure 200
      }

instance QueryLike PaginationOptions where
  toQuery options =
    toQuery
      [ ("page", show (Semigroup.getLast (paginationOptions_page options))),
        ("per_page", show (Semigroup.getLast (paginationOptions_perPage options)))
      ]
