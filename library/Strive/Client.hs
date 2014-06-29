-- | Types and functions for dealing with the API client itself.
module Strive.Client
    ( Client (..)
    , newClient
    ) where

import           Network.HTTP.Client.Conduit (Manager, newManager)
import           Strive.Types                (AccessToken)

-- | Strava V3 API Client.
data Client = Client
    { accessToken :: AccessToken
    , httpManager :: Manager
    }

-- | Create a new client using the default HTTP manager.
newClient :: AccessToken -> IO Client
newClient token = do
    manager <- newManager
    return Client
        { accessToken = token
        , httpManager = manager
        }
