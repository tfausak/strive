{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
    ( buildAuthorizeURL
    , postDeauthorize
    , postToken
    ) where

import           Data.Aeson                  (FromJSON, Value, eitherDecode)
import           Data.ByteString.Char8       (pack, unpack)
import           Data.List                   (intercalate)
import           Data.Monoid                 ((<>))
import           Network.HTTP.Client.Conduit (newManager)
import           Network.HTTP.Conduit        (checkStatus, httpLbs, method,
                                              parseUrl, responseBody)
import           Network.HTTP.Types.URI      (Query, renderQuery)
import           Strive.Client               (Client (accessToken, httpManager))
import           Strive.Objects              (DeauthorizationResponse,
                                              TokenExchangeResponse)
import           Strive.Utilities            (decodeResponse)

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeURL :: Integer -> String -> Maybe String -> Maybe [String] -> Maybe String -> String
buildAuthorizeURL clientId redirectURL approvalPrompt scope state =
    "https://www.strava.com/oauth/authorize" <> unpack (renderQuery True query)
  where
    query =
        [ ("client_id", Just (pack (show clientId)))
        , ("redirect_url", Just (pack redirectURL))
        , ("response_type", Just "code")
        , ("approval_prompt", fmap pack approvalPrompt)
        , ("scope", fmap (pack . intercalate ",") scope)
        , ("state", fmap pack state)
        ]

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
postDeauthorize :: Client -> IO (Either String DeauthorizationResponse)
postDeauthorize client = post resource query
  where
    resource = "deauthorize"
    query =
        [ ("access_token", Just (pack (accessToken client)))
        ]

-- | <http://strava.github.io/api/v3/oauth/#post-token>
postToken :: Integer -> String -> String -> IO (Either String TokenExchangeResponse)
postToken clientId clientSecret code = post resource query
  where
    resource = "token"
    query =
        [ ("client_id", Just (pack (show clientId)))
        , ("client_secret", Just (pack clientSecret))
        , ("code", Just (pack code))
        ]

post :: FromJSON a => String -> Query -> IO (Either String a)
post resource query = do
    initialRequest <- parseUrl url
    let request = initialRequest
            { checkStatus = \ _ _ _ -> Nothing
            , method = "POST"
            }
    manager <- newManager
    response <- httpLbs request manager
    return (decodeResponse response)
  where
    url = concat
        [ "https://www.strava.com/oauth/"
        , resource
        , unpack (renderQuery True query)
        ]
