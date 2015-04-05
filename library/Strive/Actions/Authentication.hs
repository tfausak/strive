-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
  ( buildAuthorizeUrl
  , exchangeToken
  , deauthorize
  ) where

import Data.ByteString.Char8 (unpack)
import Network.HTTP.Types (Query, renderQuery, toQuery)
import Strive.Aliases (Result)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (post)
import Strive.Options (BuildAuthorizeUrlOptions)
import Strive.Types (DeauthorizationResponse, TokenExchangeResponse)

-- TODO: Move to Strive.Aliases.
type ApplicationId = Integer
type RedirectUri = String
type ApplicationSecret = String
type AuthorizationCode = String

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: ApplicationId -> RedirectUri -> BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUri options =
  "https://www.strava.com/oauth/authorize" ++ unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_uri", redirectUri)
    , ("response_type", "code")
    ] ++ toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken :: ApplicationId -> ApplicationSecret -> AuthorizationCode -> Result TokenExchangeResponse
exchangeToken clientId clientSecret code = do
  client <- buildClient ""
  post client resource query
 where
  resource = "oauth/token"
  query =
    [ ("client_id", show clientId)
    , ("client_secret", clientSecret)
    , ("code", code)
    ]

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
deauthorize :: Client -> Result DeauthorizationResponse
deauthorize client = post client resource query
 where
  resource = "oauth/deauthorize"
  query = [] :: Query
