{-# LANGUAGE OverloadedStrings #-}

-- | <http://strava.github.io/api/v3/oauth/#post-token>
module Strive.Objects.Authentication.TokenExchangeResponse
    ( TokenExchangeResponse (..)
    ) where

import Control.Applicative (empty, (<$>), (<*>))
import Data.Aeson (FromJSON, Value (Object), parseJSON, (.:))
import Data.Text (Text)
import Strive.Objects.Athletes (AthleteDetailed)

-- | The expected successful response from token exchange.
data TokenExchangeResponse = TokenExchangeResponse
    { accessToken :: Text
    , athlete     :: AthleteDetailed
    } deriving Show

instance FromJSON TokenExchangeResponse where
    parseJSON (Object o) = TokenExchangeResponse
        <$> o .: "access_token"
        <*> o .: "athlete"
    parseJSON _ = empty
