-- | 'Strive.Actions.Streams'
module Strive.Options.Streams
  ( GetStreamsOptions (..),
    defaultGetStreamsOptions,
  )
where

import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (Resolution, SeriesType (Distance))

-- | 'Strive.Actions.getStreams'
data GetStreamsOptions = GetStreamsOptions
  { getStreamsOptions_resolution :: Maybe Resolution,
    getStreamsOptions_seriesType :: SeriesType
  }
  deriving (Show)

defaultGetStreamsOptions :: GetStreamsOptions
defaultGetStreamsOptions =
  GetStreamsOptions
    { getStreamsOptions_resolution = Nothing,
      getStreamsOptions_seriesType = Distance
    }

instance QueryLike GetStreamsOptions where
  toQuery options =
    toQuery
      [ ("resolution", fmap show (getStreamsOptions_resolution options)),
        ("series_type", Just (show (getStreamsOptions_seriesType options)))
      ]
