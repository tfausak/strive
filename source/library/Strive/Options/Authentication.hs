-- | 'Strive.Actions.Authentication'
module Strive.Options.Authentication
  ( BuildAuthorizeUrlOptions (..),
    defaultBuildAuthorizeUrlOptions,
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.buildAuthorizeUrl'
data BuildAuthorizeUrlOptions = BuildAuthorizeUrlOptions
  { buildAuthorizeUrlOptions_approvalPrompt :: Bool,
    buildAuthorizeUrlOptions_privateScope :: Bool,
    buildAuthorizeUrlOptions_writeScope :: Bool,
    buildAuthorizeUrlOptions_state :: String
  }
  deriving (Show)

defaultBuildAuthorizeUrlOptions :: BuildAuthorizeUrlOptions
defaultBuildAuthorizeUrlOptions =
  BuildAuthorizeUrlOptions
    { buildAuthorizeUrlOptions_approvalPrompt = False,
      buildAuthorizeUrlOptions_privateScope = False,
      buildAuthorizeUrlOptions_writeScope = False,
      buildAuthorizeUrlOptions_state = ""
    }

instance QueryLike BuildAuthorizeUrlOptions where
  toQuery options =
    toQuery $
      [ ( "approval_prompt",
          unpack
            ( toStrict
                (encode (buildAuthorizeUrlOptions_approvalPrompt options))
            )
        ),
        ("state", buildAuthorizeUrlOptions_state options)
      ]
        <> if null scopes then [] else [("scope", intercalate "," scopes)]
    where
      scopes =
        catMaybes
          [ if buildAuthorizeUrlOptions_privateScope options
              then Just "view_private"
              else Nothing,
            if buildAuthorizeUrlOptions_writeScope options
              then Just "write"
              else Nothing
          ]
