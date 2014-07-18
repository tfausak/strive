module Strive.Options.Streams
  ( GetStreamsOptions (..)
  ) where

import Data.Default (Default, def)
import Network.HTTP.Types (QueryLike, toQuery)
import Strive.Enums (Resolution, SeriesType (Distance))

-- | 'Strive.Actions.getStreams'
data GetStreamsOptions = GetStreamsOptions
  { getStreamsOptions_resolution :: Maybe Resolution
  , getStreamsOptions_seriesType :: SeriesType
  } deriving Show

instance Default GetStreamsOptions where
  def = GetStreamsOptions
    { getStreamsOptions_resolution = Nothing
    , getStreamsOptions_seriesType = Distance
    }

instance QueryLike GetStreamsOptions where
  toQuery options = toQuery
    [ ("resolution", fmap show (getStreamsOptions_resolution options))
    , ("distance", Just (show (getStreamsOptions_seriesType options)))
    ]
