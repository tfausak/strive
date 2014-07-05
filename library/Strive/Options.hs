-- | Optional parameters for actions.
module Strive.Options
    ( GetActivityOptions (..)
    ) where

import Data.Aeson (encode)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)

-- | Options for 'Strive.Actions.getActivity'.
data GetActivityOptions = GetActivityOptions
    { getActivityOptionsIncludeAllEfforts :: Bool
    }

instance Default GetActivityOptions where
    def = GetActivityOptions
        { getActivityOptionsIncludeAllEfforts = False
        }

instance QueryLike GetActivityOptions where
    toQuery options =
        [ (pack "include_all_efforts", Just (toStrict (encode (getActivityOptionsIncludeAllEfforts options))))
        ]
