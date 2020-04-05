-- | Helper functions for template Haskell, to avoid stage restrictions.
module Strive.Internal.TH
  ( options
  , makeLenses
  ) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe (isJust)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

-- | Default FromJSON options.
options :: Options
options = defaultOptions
  { fieldLabelModifier = underscore . dropPrefix
  }

underscore :: String -> String
underscore = concatMap go
 where
  go c = if isUpper c
    then ['_', toLower c]
    else [c]

dropPrefix :: String -> String
dropPrefix = drop 1 . dropWhile (/= '_')

-- | Generate lens classes and instances for a type.
makeLenses :: String -> TH.Q [TH.Dec]
makeLenses string = do
  maybeName <- TH.lookupTypeName string
  case maybeName of
    Just name -> do
      info <- TH.reify name
      case info of
        TH.TyConI (TH.DataD _ _ _ _ [TH.RecC _ triples] _) -> do
          classes <- makeLensClasses triples
          instances <- makeLensInstances name triples
          return (classes ++ instances)
        _ -> fail "reify failed"
    _ -> fail "lookupTypeName failed"

makeLensClasses :: [TH.VarStrictType] -> TH.Q [TH.Dec]
makeLensClasses [] = return []
makeLensClasses (triple : triples) = do
  exists <- lensExists triple
  if exists
    then makeLensClasses triples
    else do
      klass <- makeLensClass triple
      classes <- makeLensClasses triples
      return (klass : classes)

makeLensClass :: TH.VarStrictType -> TH.Q TH.Dec
makeLensClass triple = do
  exists <- lensExists triple
  if exists
    then fail "lens already exists"
    else do
      a <- TH.newName "a"
      b <- TH.newName "b"
      let klass = TH.ClassD [] name types dependencies declarations
          name = TH.mkName (getLensName triple)
          types = [TH.PlainTV a, TH.PlainTV b]
          dependencies = [TH.FunDep [a] [b]]
          declarations = [TH.SigD field typ]
          field = TH.mkName (getFieldName triple)
          typ = TH.AppT (TH.AppT (TH.ConT (TH.mkName "Lens")) (TH.VarT a)) (TH.VarT b)
      return klass

lensExists :: TH.VarStrictType -> TH.Q Bool
lensExists triple = do
  let name = getLensName triple
  maybeName <- TH.lookupTypeName name
  return (isJust maybeName)

getLensName :: TH.VarStrictType -> String
getLensName triple = capitalize (getFieldName triple) ++ "Lens"

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

getFieldName :: TH.VarStrictType -> String
getFieldName (var, _, _) = (lensName . show) var

lensName :: String -> String
lensName x = if y `elem` keywords then y ++ "_" else y
 where
  y = dropPrefix x
  keywords = ["data", "type"]

makeLensInstances :: TH.Name -> [TH.VarStrictType] -> TH.Q [TH.Dec]
makeLensInstances name triples = mapM (makeLensInstance name) triples

makeLensInstance :: TH.Name -> TH.VarStrictType -> TH.Q TH.Dec
makeLensInstance name triple@(var, _, typ) = do
  f <- TH.newName "f"
  x <- TH.newName "x"
  a <- TH.newName "a"
  Just fmap' <- TH.lookupValueName "fmap"
  let field = TH.mkName (getFieldName triple)
  return $ TH.InstanceD
    Nothing
    []
    (TH.AppT (TH.AppT (TH.ConT (TH.mkName (getLensName triple))) (TH.ConT name)) typ)
    [TH.FunD field [TH.Clause [TH.VarP f, TH.VarP x] (TH.NormalB (TH.AppE (TH.AppE (TH.VarE fmap') (TH.LamE [TH.VarP a] (TH.RecUpdE (TH.VarE x) [(var, TH.VarE a)]))) (TH.AppE (TH.VarE f) (TH.AppE (TH.VarE var) (TH.VarE x))))) []]]
