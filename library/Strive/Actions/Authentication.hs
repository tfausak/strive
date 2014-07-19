-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
  ( buildAuthorizeUrl
  , exchangeToken
  , deauthorize
  ) where

import Data.ByteString.Char8 (unpack)
import Network.HTTP.Types (Query, renderQuery, toQuery)
import Strive.Client (Client, buildClient)
import Strive.Internal.HTTP (post)
import Strive.Options (BuildAuthorizeUrlOptions)
import Strive.Types (DeauthorizationResponse, TokenExchangeResponse)

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeUrl :: Integer -> String -> BuildAuthorizeUrlOptions -> String
buildAuthorizeUrl clientId redirectUri options =
  "https://www.strava.com/oauth/authorize" ++ unpack (renderQuery True query)
 where
  query = toQuery
    [ ("client_id", show clientId)
    , ("redirect_uri", redirectUri)
    , ("response_type", "code")
    ] ++ toQuery options

-- | <http://strava.github.io/api/v3/oauth/#post-token>
exchangeToken :: Integer -> String -> String -> IO (Either String TokenExchangeResponse)
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
deauthorize :: Client -> IO (Either String DeauthorizationResponse)
deauthorize client = post client resource query
 where
  resource = "oauth/deauthorize"
  query = [] :: Query
