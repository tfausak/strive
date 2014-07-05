{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Strive.Client where

import Network.HTTP.Client.Conduit (Manager, newManager)
import Strive.Lenses

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

-- TODO

instance AccessTokenLens Client String where
  accessToken client =
    ( client_accessToken client
    , \ accessToken' -> client { client_accessToken = accessToken' }
    )

instance HttpManagerLens Client Manager where
  httpManager client =
    ( client_httpManager client
    , \ httpManager' -> client { client_httpManager = httpManager' }
    )
