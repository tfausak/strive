{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
    ( buildAuthorizeURL
    ) where

import           Data.ByteString.Char8  (pack)
import Data.List (intercalate)
import           Network.HTTP.Types.URI (renderQuery)

-- | <http://strava.github.io/api/v3/oauth/#get-authorize>
buildAuthorizeURL :: Integer -> String -> String -> Maybe String -> Maybe [String] -> Maybe String -> String
buildAuthorizeURL clientId redirectURL responseType approvalPrompt scope state = concat
    [ "https://www.strava.com/oauth/authorize"
    , renderQuery True query
    ]
  where
    query =
        [ ("client_id", Just (show clientId))
        , ("redirect_url", Just (pack redirectURL))
        , ("response_type", Just (pack responseType))
        , ("approval_prompt", fmap pack approvalPrompt)
        , ("scope", fmap (pack . intercalate ",") scope)
        , ("state", fmap pack state)
        ]
