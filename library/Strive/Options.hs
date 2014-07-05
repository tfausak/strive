{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | Optional parameters for actions.
module Strive.Options where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.Default (Default, def)
import Network.HTTP.Types.QueryLike (QueryLike, toQuery)
import Strive.Lens

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
    [ ("approval_prompt", unpack (toStrict (encode (get approvalPrompt options))))
    , ("scope", scopes)
    , ("state", get state options)
    ]
   where
    scopes = unwords
      [ if get privateScope options then "view_private" else ""
      , if get writeScope options then "write" else ""
      ]

-- TODO: Everything below here should be generated with metaprogramming.

instance ApprovalPromptLens BuildAuthorizeUrlOptions Bool where
  approvalPrompt options =
    ( buildAuthorizeUrlOptions_approvalPrompt options
    , \ approvalPrompt' -> options { buildAuthorizeUrlOptions_approvalPrompt = approvalPrompt' }
    )

instance PrivateScopeLens BuildAuthorizeUrlOptions Bool where
  privateScope options =
    ( buildAuthorizeUrlOptions_privateScope options
    , \ privateScope' -> options { buildAuthorizeUrlOptions_privateScope = privateScope' }
    )

instance StateLens BuildAuthorizeUrlOptions String where
  state options =
    ( buildAuthorizeUrlOptions_state options
    , \ state' -> options { buildAuthorizeUrlOptions_state = state' }
    )

instance WriteScopeLens BuildAuthorizeUrlOptions Bool where
  writeScope options =
    ( buildAuthorizeUrlOptions_writeScope options
    , \ writeScope' -> options { buildAuthorizeUrlOptions_writeScope = writeScope' }
    )
