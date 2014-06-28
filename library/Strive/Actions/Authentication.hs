{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
    ( buildAuthorizeURL
    , postDeauthorize
    , postToken
    ) where

import           Data.Aeson                  (Value, eitherDecode)
import           Data.ByteString.Char8       (pack, unpack)
import           Data.List                   (intercalate)
import           Data.Monoid                 ((<>))
import           Network.HTTP.Client.Conduit (newManager)
import           Network.HTTP.Conduit        (checkStatus, httpLbs, method,
                                              parseUrl, responseBody)
import           Network.HTTP.Types.URI      (renderQuery)
import           Strive.Client               (Client (accessToken, httpManager))

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
postDeauthorize :: Client -> IO (Either String Value)
postDeauthorize client = do
    initialRequest <- parseUrl url
    let request = initialRequest
            { checkStatus = \ _ _ _ -> Nothing
            , method = "POST"
            }
    response <- httpLbs request (httpManager client)
    return (eitherDecode (responseBody response))
  where
    url = "https://www.strava.com/oauth/deauthorize"
    query =
        [ ("access_token", Just (pack (accessToken client)))
        ]

-- | <http://strava.github.io/api/v3/oauth/#post-token>
postToken :: Integer -> String -> String -> IO (Either String Value)
postToken clientId clientSecret code = do
    initialRequest <- parseUrl url
    let request = initialRequest
            { checkStatus = \ _ _ _ -> Nothing
            , method = "POST"
            }
    manager <- newManager
    response <- httpLbs request manager
    return (eitherDecode (responseBody response))
  where
    url = "https://www.strava.com/oauth/token" <> unpack (renderQuery True query)
    query =
        [ ("client_id", Just (pack (show clientId)))
        , ("client_secret", Just (pack clientSecret))
        , ("code", Just (pack code))
        ]
