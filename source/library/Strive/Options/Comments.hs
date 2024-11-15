-- | 'Strive.Actions.Comments'
module Strive.Options.Comments
  ( GetActivityCommentsOptions (..),
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Semigroup as Semigroup
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.getActivityComments'
data GetActivityCommentsOptions = GetActivityCommentsOptions
  { getActivityCommentsOptions_markdown :: Semigroup.Last Bool,
    getActivityCommentsOptions_page :: Semigroup.Last Integer,
    getActivityCommentsOptions_perPage :: Semigroup.Last Integer
  }
  deriving (Show)

instance Semigroup GetActivityCommentsOptions where
  x <> y =
    GetActivityCommentsOptions
      { getActivityCommentsOptions_markdown = getActivityCommentsOptions_markdown x <> getActivityCommentsOptions_markdown y,
        getActivityCommentsOptions_page = getActivityCommentsOptions_page x <> getActivityCommentsOptions_page y,
        getActivityCommentsOptions_perPage = getActivityCommentsOptions_perPage x <> getActivityCommentsOptions_perPage y
      }

instance Monoid GetActivityCommentsOptions where
  mempty =
    GetActivityCommentsOptions
      { getActivityCommentsOptions_markdown = pure False,
        getActivityCommentsOptions_page = pure 1,
        getActivityCommentsOptions_perPage = pure 200
      }

instance QueryLike GetActivityCommentsOptions where
  toQuery options =
    toQuery
      [ ( "before",
          unpack
            (toStrict (encode (Semigroup.getLast (getActivityCommentsOptions_markdown options))))
        ),
        ("page", show (Semigroup.getLast (getActivityCommentsOptions_page options))),
        ("per_page", show (Semigroup.getLast (getActivityCommentsOptions_perPage options)))
      ]
