-- | <http://strava.github.io/api/v3/comments/>
module Strive.Actions.Comments
    ( getActivityComments
    ) where

import           Data.Aeson            (encode)
import           Data.ByteString.Char8 (pack)
import           Data.ByteString.Lazy  (toStrict)
import           Data.Monoid           ((<>))
import           Strive.Client         (Client)
import           Strive.Objects        (CommentSummary)
import           Strive.Types          (ActivityId, IncludeMarkdown, Page,
                                        PerPage)
import           Strive.Utilities      (get, paginate, queryToSimpleQuery)

-- | <http://strava.github.io/api/v3/comments/#list>
getActivityComments :: Client -> ActivityId -> IncludeMarkdown -> Page -> PerPage -> IO (Either String [CommentSummary])
getActivityComments client activityId includeMarkdown page perPage = get client resource query
  where
    resource = "activities/" <> show activityId <> "/comments"
    query = paginate page perPage <> queryToSimpleQuery
        [ (pack "markdown", fmap (toStrict . encode) includeMarkdown)
        ]
