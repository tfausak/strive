-- | Optional parameters for actions.
module Strive.Options where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)

-- * Authentication

-- | Options for 'Strive.Actions.buildAuthorizeUrl'.
data BuildAuthorizeUrlOptions = BuildAuthorizeUrlOptions
  { buildAuthorizeUrlOptions_approvalPrompt :: Bool
  , buildAuthorizeUrlOptions_privateScope   :: Bool
  , buildAuthorizeUrlOptions_writeScope     :: Bool
  , buildAuthorizeUrlOptions_state          :: String
  } deriving Show

instance Default BuildAuthorizeUrlOptions where
  def = BuildAuthorizeUrlOptions
    { buildAuthorizeUrlOptions_approvalPrompt = False
    , buildAuthorizeUrlOptions_privateScope = False
    , buildAuthorizeUrlOptions_writeScope = False
    , buildAuthorizeUrlOptions_state = ""
    }

instance QueryLike BuildAuthorizeUrlOptions where
  toQuery options = toQuery
    [ ("approval_prompt", unpack (toStrict (encode (buildAuthorizeUrlOptions_approvalPrompt options))))
    , ("scope", scopes)
    , ("state", buildAuthorizeUrlOptions_state options)
    ]
   where
    scopes = unwords
      [ if buildAuthorizeUrlOptions_privateScope options then "view_private" else ""
      , if buildAuthorizeUrlOptions_writeScope options then "write" else ""
      ]

-- * Athletes

-- | Options for 'Strive.Actions.updateCurrentAthlete'.
data UpdateCurrentAthleteOptions = UpdateCurrentAthleteOptions
  { updateCurrentAthleteOptions_city    :: Maybe String
  , updateCurrentAthleteOptions_state   :: Maybe String
  , updateCurrentAthleteOptions_country :: Maybe String
  , updateCurrentAthleteOptions_sex     :: Maybe Char
  , updateCurrentAthleteOptions_weight  :: Maybe Double
  } deriving Show

instance Default UpdateCurrentAthleteOptions where
  def = UpdateCurrentAthleteOptions
    { updateCurrentAthleteOptions_city = Nothing
    , updateCurrentAthleteOptions_state = Nothing
    , updateCurrentAthleteOptions_country = Nothing
    , updateCurrentAthleteOptions_sex = Nothing
    , updateCurrentAthleteOptions_weight = Nothing
    }

instance QueryLike UpdateCurrentAthleteOptions where
  toQuery options = toQuery
    [ ("city", updateCurrentAthleteOptions_city options)
    , ("state", updateCurrentAthleteOptions_state options)
    , ("country", updateCurrentAthleteOptions_country options)
    , ("sex", fmap (: []) (updateCurrentAthleteOptions_sex options))
    , ("weight", fmap show (updateCurrentAthleteOptions_weight options))
    ]

-- | Options for 'Strive.Actions.getAthleteCrs'.
data GetAthleteCrsOptions = GetAthleteCrsOptions
  { getAthleteCrsOptions_page    :: Integer
  , getAthleteCrsOptions_perPage :: Integer
  } deriving Show

instance Default GetAthleteCrsOptions where
  def = GetAthleteCrsOptions
    { getAthleteCrsOptions_page = 1
    , getAthleteCrsOptions_perPage = 200
    }

instance QueryLike GetAthleteCrsOptions where
  toQuery options = toQuery
    [ ("page", show (getAthleteCrsOptions_page options))
    , ("per_page", show (getAthleteCrsOptions_perPage options))
    ]
