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
