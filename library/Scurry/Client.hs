-- | Types and functions for dealing with the API client itself.
module Scurry.Client
    ( Client (..)
    ) where

import           Network.HTTP.Conduit (Manager)

-- | Strava V3 API Client.
data Client = Client
    { accessToken :: String
    , httpManager :: Manager
    }
