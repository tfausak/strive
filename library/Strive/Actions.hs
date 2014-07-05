-- | Functions for performing actions against the API.
module Strive.Actions where

import Data.ByteString.Char8 (unpack)
import Data.Default (Default, def)
import Data.Monoid ((<>))
import Network.HTTP.Types (Query, renderQuery, toQuery)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (get, post)
import qualified Strive.Options as O
import qualified Strive.Types as T

-- | Helper function for easily performing actions.
with :: Default a => [a -> a] -> a
with = foldr ($) def

-- * Authentication

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: Integer -> String -> O.BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUrl options =
  "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_url", redirectUrl)
    , ("response_type", "code")
    ] <> toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken :: Integer -> String -> String -> IO (Either String T.TokenExchangeResponse)
exchangeToken clientId clientSecret code = do
  client <- buildClient "" -- TODO: This is kind of dumb.
  post client resource query
 where
  resource = "oauth/token"
  query =
    [ ("client_id", show clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    ]

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
deauthorize :: Client -> IO (Either String T.DeauthorizationResponse)
deauthorize client = post client resource query
 where
  resource = "oauth/deauthorize"
  query = [] :: Query

-- * Athletes

-- | <http://strava.github.io/api/v3/athlete/#get-details>
getCurrentAthlete :: Client -> IO (Either String T.AthleteDetailed)
getCurrentAthlete client = get client resource query
 where
  resource = "api/v3/athlete"
  query = [] :: Query

-- | <http://strava.github.io/api/v3/athlete/#get-another-details>
getAthlete :: Client -> Integer -> IO (Either String T.AthleteSummary)
getAthlete client athleteId = get client resource query
  where
    resource = "api/v3/athletes/" <> show athleteId
    query = [] :: Query
