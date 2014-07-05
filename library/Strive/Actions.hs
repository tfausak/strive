-- | Functions for performing actions against the API.
module Strive.Actions where

import Data.ByteString.Char8 (unpack)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Network.HTTP.Types (renderQuery, toQuery)
import Strive.Options (BuildAuthorizeUrlOptions)

-- | Helper function for easily performing actions.
with :: Default a => [a -> a] -> a
with = foldr ($) def

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: Integer -> String -> BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUrl options =
  "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_url", redirectUrl)
    , ("response_type", "code")
    ] <> toQuery options
