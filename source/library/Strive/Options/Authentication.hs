-- | 'Strive.Actions.Authentication'
module Strive.Options.Authentication
  ( BuildAuthorizeUrlOptions (..),
  )
where

import Data.Aeson (encode)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import qualified Data.Semigroup as Semigroup
import Network.HTTP.Types (QueryLike, toQuery)

-- | 'Strive.Actions.buildAuthorizeUrl'
data BuildAuthorizeUrlOptions = BuildAuthorizeUrlOptions
  { buildAuthorizeUrlOptions_approvalPrompt :: Semigroup.Last Bool,
    buildAuthorizeUrlOptions_privateScope :: Semigroup.Last Bool,
    buildAuthorizeUrlOptions_writeScope :: Semigroup.Last Bool,
    buildAuthorizeUrlOptions_state :: Semigroup.Last String
  }
  deriving (Show)

instance Semigroup BuildAuthorizeUrlOptions where
  x <> y =
    BuildAuthorizeUrlOptions
      { buildAuthorizeUrlOptions_approvalPrompt = buildAuthorizeUrlOptions_approvalPrompt x <> buildAuthorizeUrlOptions_approvalPrompt y,
        buildAuthorizeUrlOptions_privateScope = buildAuthorizeUrlOptions_privateScope x <> buildAuthorizeUrlOptions_privateScope y,
        buildAuthorizeUrlOptions_writeScope = buildAuthorizeUrlOptions_writeScope x <> buildAuthorizeUrlOptions_writeScope y,
        buildAuthorizeUrlOptions_state = buildAuthorizeUrlOptions_state x <> buildAuthorizeUrlOptions_state y
      }

instance Monoid BuildAuthorizeUrlOptions where
  mempty =
    BuildAuthorizeUrlOptions
      { buildAuthorizeUrlOptions_approvalPrompt = pure False,
        buildAuthorizeUrlOptions_privateScope = pure False,
        buildAuthorizeUrlOptions_writeScope = pure False,
        buildAuthorizeUrlOptions_state = pure ""
      }

instance QueryLike BuildAuthorizeUrlOptions where
  toQuery options =
    toQuery $
      [ ( "approval_prompt",
          unpack
            ( toStrict
                (encode (Semigroup.getLast (buildAuthorizeUrlOptions_approvalPrompt options)))
            )
        ),
        ("state", Semigroup.getLast (buildAuthorizeUrlOptions_state options))
      ]
        <> if null scopes then [] else [("scope", intercalate "," scopes)]
    where
      scopes =
        catMaybes
          [ if Semigroup.getLast (buildAuthorizeUrlOptions_privateScope options)
              then Just "view_private"
              else Nothing,
            if Semigroup.getLast (buildAuthorizeUrlOptions_writeScope options)
              then Just "write"
              else Nothing
          ]
