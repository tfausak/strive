-- | 'Strive.Actions.Streams'
module Strive.Options.Streams
  ( GetStreamsOptions (..),
  )
where

import qualified Data.Monoid as Monoid
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (Resolution, SeriesType)

-- | 'Strive.Actions.getStreams'
data GetStreamsOptions = GetStreamsOptions
  { getStreamsOptions_resolution :: Monoid.Last Resolution,
    getStreamsOptions_seriesType :: Monoid.Last SeriesType
  }
  deriving (Show)

instance Semigroup GetStreamsOptions where
  x <> y =
    GetStreamsOptions
      { getStreamsOptions_resolution = getStreamsOptions_resolution x <> getStreamsOptions_resolution y,
        getStreamsOptions_seriesType = getStreamsOptions_seriesType x <> getStreamsOptions_seriesType y
      }

instance Monoid GetStreamsOptions where
  mempty =
    GetStreamsOptions
      { getStreamsOptions_resolution = mempty,
        getStreamsOptions_seriesType = mempty
      }

instance QueryLike GetStreamsOptions where
  toQuery options =
    toQuery
      [ fmap ((,) "resolution" . show) . Monoid.getLast $ getStreamsOptions_resolution options,
        fmap ((,) "series_type" . show) . Monoid.getLast $ getStreamsOptions_seriesType options
      ]
