{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Strive.Types.Authentication
  ( TokenExchangeResponse (..)
  , DeauthorizationResponse (..)
  ) where

import Data.Aeson.TH (deriveFromJSON)
import Data.Text (Text)
import Strive.Internal.TH (options)
import Strive.Types.Athletes (AthleteDetailed)

-- | <http://strava.github.io/api/v3/oauth/#example-response>
data TokenExchangeResponse = TokenExchangeResponse
  { tokenExchangeResponse_accessToken :: Text
  , tokenExchangeResponse_athlete     :: AthleteDetailed
  } deriving Show

$(deriveFromJSON options ''TokenExchangeResponse)

-- | <http://strava.github.io/api/v3/oauth/#example-response-1>
data DeauthorizationResponse = DeauthorizationResponse
  { deauthorizationResponse_accessToken :: Text
  } deriving Show

$(deriveFromJSON options ''DeauthorizationResponse)
