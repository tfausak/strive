-- | 'Strive.Actions.Comments'
module Strive.Options.Comments
  ( GetActivityCommentsOptions (..),
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Monoid as Monoid
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.getActivityComments'
data GetActivityCommentsOptions = GetActivityCommentsOptions
  { getActivityCommentsOptions_markdown :: Monoid.Last Bool,
    getActivityCommentsOptions_page :: Monoid.Last Integer,
    getActivityCommentsOptions_perPage :: Monoid.Last Integer
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
      [ fmap ((,) "before" . unpack . toStrict . encode) . Monoid.getLast $ getActivityCommentsOptions_markdown options,
        fmap ((,) "page" . show) . Monoid.getLast $ getActivityCommentsOptions_page options,
        fmap ((,) "per_page" . show) . Monoid.getLast $ getActivityCommentsOptions_perPage options
      ]
