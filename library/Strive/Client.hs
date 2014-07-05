-- | Types and functions for dealing with the API client itself.
module Strive.Client where

import Network.HTTP.Client.Conduit (Manager, newManager)

-- | Strava V3 API client.
data Client = Client
  { client_accessToken :: String
  , client_httpManager :: Manager
  }

-- | Build a client with the default HTTP manager.
buildClient :: String -> IO Client
buildClient token = do
  manager <- newManager
  return Client
    { client_accessToken = token
    , client_httpManager = manager
    }
