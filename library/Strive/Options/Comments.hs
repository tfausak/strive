-- | <http://strava.github.io/api/v3/comments/>
module Strive.Options.Comments
  ( GetActivityCommentsOptions (..)
  ) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.getActivityComments'
data GetActivityCommentsOptions = GetActivityCommentsOptions
  { getActivityCommentsOptions_markdown :: Bool
  , getActivityCommentsOptions_page     :: Integer
  , getActivityCommentsOptions_perPage  :: Integer
  } deriving Show

instance Default GetActivityCommentsOptions where
  def = GetActivityCommentsOptions
    { getActivityCommentsOptions_markdown = False
    , getActivityCommentsOptions_page = 1
    , getActivityCommentsOptions_perPage = 200
    }

instance QueryLike GetActivityCommentsOptions where
  toQuery options = toQuery
    [ ("before", unpack (toStrict (encode (getActivityCommentsOptions_markdown options))))
    , ("page", show (getActivityCommentsOptions_page options))
    , ("per_page", show (getActivityCommentsOptions_perPage options))
    ]
