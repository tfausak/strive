-- | Helper functions for template Haskell, to avoid stage restrictions.
module Strive.Internal.TH
  ( options
  , makeLenses
  ) where

import Data.Aeson.TH (Options, defaultOptions, fieldLabelModifier)
import Data.Char (isUpper, toLower)

import Data.Char (toUpper)
import Data.Maybe (isJust)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

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
dropPrefix = tail . dropWhile (/= '_')

--

makeLenses :: String -> Q [Dec]
makeLenses string = do
  maybeName <- lookupTypeName string
  case maybeName of
    Just name -> do
      info <- reify name
      case info of
        TyConI (DataD _ _ _ [RecC _ triples] _) -> do
          classes <- makeLensClasses triples
          instances <- makeLensInstances name triples
          return (classes ++ instances)
        _ -> fail "reify failed"
    _ -> fail "lookupTypeName failed"

makeLensClasses :: [VarStrictType] -> Q [Dec]
makeLensClasses [] = return []
makeLensClasses (triple : triples) = do
  exists <- lensExists triple
  if exists
    then makeLensClasses triples
    else do
      klass <- makeLensClass triple
      classes <- makeLensClasses triples
      return (klass : classes)

makeLensClass :: VarStrictType -> Q Dec
makeLensClass triple = do
  exists <- lensExists triple
  if exists
    then fail "lens already exists"
    else do
      a <- newName "a"
      b <- newName "b"
      let klass = ClassD context name types dependencies declarations
          context = []
          name = mkName (getLensName triple)
          types = [PlainTV a, PlainTV b]
          dependencies = [FunDep [a] [b]]
          declarations = [SigD field typ]
          field = mkName (getFieldName triple)
          typ = AppT (AppT (ConT (mkName "Lens")) (VarT a)) (VarT b)
      return klass

lensExists :: VarStrictType -> Q Bool
lensExists triple = do
  let name = getLensName triple
  maybeName <- lookupTypeName name
  return (isJust maybeName)

getLensName :: VarStrictType -> String
getLensName triple = capitalize (getFieldName triple) ++ "Lens"

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

getFieldName :: VarStrictType -> String
getFieldName (var, _, _) = (drop 1 . dropWhile (/= '_') . show) var

makeLensInstances :: Name -> [VarStrictType] -> Q [Dec]
makeLensInstances name triples = mapM (makeLensInstance name) triples

makeLensInstance :: Name -> VarStrictType -> Q Dec
makeLensInstance name triple@(var, _, typ) = do
  f <- newName "f"
  x <- newName "x"
  a <- newName "a"
  Just fmap' <- lookupValueName "fmap"
  let field = mkName (getFieldName triple)
  return $ InstanceD
    []
    (AppT (AppT (ConT (mkName (getLensName triple))) (ConT name)) typ)
    [FunD field [Clause [VarP f, VarP x] (NormalB (AppE (AppE (VarE fmap') (LamE [VarP a] (RecUpdE (VarE x) [(var, VarE a)]))) (AppE (VarE f) (AppE (VarE var) (VarE x))))) []]]
