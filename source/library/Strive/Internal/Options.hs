-- | Common options that apply to many endpoints.
module Strive.Internal.Options
  ( PaginationOptions (..),
  )
where

import qualified Data.Monoid as Monoid
import Network.HTTP.Types (QueryLike, toQuery)

-- | Options for paginating.
data PaginationOptions = PaginationOptions
  { paginationOptions_page :: Monoid.Last Integer,
    paginationOptions_perPage :: Monoid.Last Integer
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
      { paginationOptions_page = mempty,
        paginationOptions_perPage = mempty
      }

instance QueryLike PaginationOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "page" . show) . Monoid.getLast $ paginationOptions_page options,
        fmap ((,) "per_page" . show) . Monoid.getLast $ paginationOptions_perPage options
      ]
