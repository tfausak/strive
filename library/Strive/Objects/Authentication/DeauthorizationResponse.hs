{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/#deauthorize>
module Strive.Objects.Authentication.DeauthorizationResponse
    ( DeauthorizationResponse (..)
    ) where

import Control.Applicative (empty, (<$>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)

-- | The expected successful response from deauthorization.
data DeauthorizationResponse = DeauthorizationResponse
    { accessToken :: Text
    } deriving Show

instance FromJSON DeauthorizationResponse where
    parseJSON (Object o) = DeauthorizationResponse
        <$> o .: "access_token"
    parseJSON _ = empty
