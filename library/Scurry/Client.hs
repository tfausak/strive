{- |
    Types and functions for dealing with the API client itself.

    >>> Client "..."
    Client {accessToken = "..."}
-}
module Scurry.Client
    ( Client (..)
    ) where

-- | Strava V3 API Client.
data Client = Client
    { accessToken :: String
    } deriving (Show)
