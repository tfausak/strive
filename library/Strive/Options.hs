-- | Optional parameters for actions.
module Strive.Options where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)

-- * Authentication

-- | 'Strive.Actions.buildAuthorizeUrl'.
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

-- | 'Strive.Actions.updateCurrentAthlete'.
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

-- | 'Strive.Actions.getAthleteCrs'.
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

-- * Friends and Followers

-- | 'Strive.Actions.getCurrentFriends'
data GetCurrentFriendsOptions = GetCurrentFriendsOptions
  { getCurrentFriendsOptions_page    :: Integer
  , getCurrentFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetCurrentFriendsOptions where
  def = GetCurrentFriendsOptions
    { getCurrentFriendsOptions_page = 1
    , getCurrentFriendsOptions_perPage = 200
    }

instance QueryLike GetCurrentFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getCurrentFriendsOptions_page options))
    , ("per_page", show (getCurrentFriendsOptions_perPage options))
    ]

-- | 'Strive.Actions.getFriends'
data GetFriendsOptions = GetFriendsOptions
  { getFriendsOptions_page    :: Integer
  , getFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetFriendsOptions where
  def = GetFriendsOptions
    { getFriendsOptions_page = 1
    , getFriendsOptions_perPage = 200
    }

instance QueryLike GetFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getFriendsOptions_page options))
    , ("per_page", show (getFriendsOptions_perPage options))
    ]

-- | 'Strive.Actions.getCurrentFollowers'
data GetCurrentFollowersOptions = GetCurrentFollowersOptions
  { getCurrentFollowersOptions_page    :: Integer
  , getCurrentFollowersOptions_perPage :: Integer
  } deriving Show

instance Default GetCurrentFollowersOptions where
  def = GetCurrentFollowersOptions
    { getCurrentFollowersOptions_page = 1
    , getCurrentFollowersOptions_perPage = 200
    }

instance QueryLike GetCurrentFollowersOptions where
  toQuery options = toQuery
    [ ("page", show (getCurrentFollowersOptions_page options))
    , ("per_page", show (getCurrentFollowersOptions_perPage options))
    ]

-- | 'Strive.Actions.getFollowers'
data GetFollowersOptions = GetFollowersOptions
  { getFollowersOptions_page    :: Integer
  , getFollowersOptions_perPage :: Integer
  } deriving Show

instance Default GetFollowersOptions where
  def = GetFollowersOptions
    { getFollowersOptions_page = 1
    , getFollowersOptions_perPage = 200
    }

instance QueryLike GetFollowersOptions where
  toQuery options = toQuery
    [ ("page", show (getFollowersOptions_page options))
    , ("per_page", show (getFollowersOptions_perPage options))
    ]

-- | 'Strive.Actions.getCommonFriends'
data GetCommonFriendsOptions = GetCommonFriendsOptions
  { getCommonFriendsOptions_page    :: Integer
  , getCommonFriendsOptions_perPage :: Integer
  } deriving Show

instance Default GetCommonFriendsOptions where
  def = GetCommonFriendsOptions
    { getCommonFriendsOptions_page = 1
    , getCommonFriendsOptions_perPage = 200
    }

instance QueryLike GetCommonFriendsOptions where
  toQuery options = toQuery
    [ ("page", show (getCommonFriendsOptions_page options))
    , ("per_page", show (getCommonFriendsOptions_perPage options))
    ]

-- * Activities

-- | 'Strive.Actions.CreateActivity'
data CreateActivityOptions = CreateActivityOptions
  { createActivityOptions_description :: Maybe String
  , createActivityOptions_distance    :: Maybe Double
  } deriving Show

instance Default CreateActivityOptions where
  def = CreateActivityOptions
    { createActivityOptions_description = Nothing
    , createActivityOptions_distance = Nothing
    }

instance QueryLike CreateActivityOptions where
  toQuery options = toQuery
    [ ("description", createActivityOptions_description options)
    , ("distance", fmap show (createActivityOptions_distance options))
    ]

-- | 'Strive.Actions.GetActivity'
data GetActivityOptions = GetActivityOptions
  { createActivityOptions_allEfforts :: Bool
  } deriving Show

instance Default GetActivityOptions where
  def = GetActivityOptions
    { createActivityOptions_allEfforts = False
    }

instance QueryLike GetActivityOptions where
  toQuery options = toQuery
    [ ("approval_prompt", unpack (toStrict (encode (createActivityOptions_allEfforts options))))
    ]
