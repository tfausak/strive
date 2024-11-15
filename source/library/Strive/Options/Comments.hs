-- | 'Strive.Actions.Comments'
module Strive.Options.Comments
  ( GetActivityCommentsOptions (..),
    defaultGetActivityCommentsOptions,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.getActivityComments'
data GetActivityCommentsOptions = GetActivityCommentsOptions
  { getActivityCommentsOptions_markdown :: Bool,
    getActivityCommentsOptions_page :: Integer,
    getActivityCommentsOptions_perPage :: Integer
  }
  deriving (Show)

defaultGetActivityCommentsOptions :: GetActivityCommentsOptions
defaultGetActivityCommentsOptions =
  GetActivityCommentsOptions
    { getActivityCommentsOptions_markdown = False,
      getActivityCommentsOptions_page = 1,
      getActivityCommentsOptions_perPage = 200
    }

instance QueryLike GetActivityCommentsOptions where
  toQuery options =
    toQuery
      [ ( "before",
          unpack
            (toStrict (encode (getActivityCommentsOptions_markdown options)))
        ),
        ("page", show (getActivityCommentsOptions_page options)),
        ("per_page", show (getActivityCommentsOptions_perPage options))
      ]
