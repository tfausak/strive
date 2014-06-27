{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/>
module Strive.Actions.Authentication
    ( buildAuthorizeURL
    ) where

import           Data.ByteString.Char8  (pack, unpack)
import           Data.List              (intercalate)
import           Data.Monoid            ((<>))
import           Network.HTTP.Types.URI (renderQuery)

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
