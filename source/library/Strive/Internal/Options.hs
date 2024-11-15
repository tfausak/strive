-- | Common options that apply to many endpoints.
module Strive.Internal.Options
  ( PaginationOptions (..),
    defaultPaginationOptions,
  )
where

import Network.HTTP.Types (QueryLike, toQuery)

-- | Options for paginating.
data PaginationOptions = PaginationOptions
  { paginationOptions_page :: Integer,
    paginationOptions_perPage :: Integer
  }
  deriving (Show)

defaultPaginationOptions :: PaginationOptions
defaultPaginationOptions =
  PaginationOptions
    { paginationOptions_page = 1,
      paginationOptions_perPage = 200
    }

instance QueryLike PaginationOptions where
  toQuery options =
    toQuery
      [ ("page", show (paginationOptions_page options)),
        ("per_page", show (paginationOptions_perPage options))
      ]
