import Data.Char (toUpper)
import Data.List (nub, sort)
import Data.Maybe (mapMaybe)
import System.FilePath.Glob (glob)
import Text.Regex (Regex, matchRegex, mkRegex)

main :: IO ()
main = do
  paths <- glob "library/**/*.hs"
  contents <- mapM readFile paths
  let matches = getMatches contents
      classes = buildClasses matches
      instances = buildInstances matches
  writeClasses classes
  writeInstances instances

getMatches :: [String] -> [(String, String, String)]
getMatches = mapMaybe match . lines . unlines

match :: String -> Maybe (String, String, String)
match string = case matchRegex pattern string of
  Just [a, b, c] -> Just (a, b, c)
  _ -> Nothing

pattern :: Regex
pattern = mkRegex "[{,] *([^_]+)_([^ ]+) *:: *(.+)"

buildClasses :: [(String, String, String)] -> [String]
buildClasses = sort . nub . map buildClass

buildClass :: (String, String, String) -> String
buildClass (_, field, _) = concat
  [ "class "
  , lensName field
  , " a b | a -> b where "
  , disambiguate field
  , " :: Lens a b"
  ]

lensName :: String -> String
lensName field = capitalize field ++ "Lens"

capitalize :: String -> String
capitalize (c : s) = toUpper c : s
capitalize s = s

disambiguate :: String -> String
disambiguate s
  | isAmbiguous s = s ++ "_"
  | otherwise = s

isAmbiguous :: String -> Bool
isAmbiguous s = s `elem` keywords ++ prelude

keywords :: [String]
keywords =
  [ "data"
  , "type"
  ]

prelude :: [String]
prelude =
  [ "error"
  , "id"
  , "map"
  , "max"
  , "min"
  ]

buildInstances :: [(String, String, String)] -> [String]
buildInstances = sort . map buildInstance

buildInstance :: (String, String, String) -> String
buildInstance (recordType, field, fieldType) = concat
  [ "instance "
  , lensName field
  , " "
  , capitalize recordType
  , " "
  , wrap fieldType
  , " where "
  , disambiguate field
  , " f x = fmap (\\ y -> x { "
  , fullField
  , " = y }) (f ("
  , fullField
  , " x))"
  ]
 where
  fullField = recordType ++ "_" ++ field

wrap :: String -> String
wrap s
  | ' ' `elem` s = "(" ++ s ++ ")"
  | otherwise = s

writeClasses :: [String] -> IO ()
writeClasses classes = writeFile "library/Strive/Lenses/Classes.hs" contents
 where
  contents = unlines
    [ "{-# LANGUAGE FunctionalDependencies #-}"
    , "module Strive.Lenses.Classes where"
    , "import Strive.Lenses (Lens)"
    ] ++ unlines classes

writeInstances :: [String] -> IO ()
writeInstances instances = writeFile "library/Strive/Lenses/Instances.hs" contents
 where
  contents = unlines
    [ "{-# LANGUAGE FlexibleInstances #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , "module Strive.Lenses.Instances where"
    , "import Data.Aeson (Value)"
    , "import Data.ByteString.Lazy (ByteString)"
    , "import Data.Text (Text)"
    , "import Data.Time.Clock (UTCTime)"
    , "import Network.HTTP.Conduit (Manager, Request, Response)"
    , "import Strive.Client"
    , "import Strive.Enums"
    , "import Strive.Internal.Options"
    , "import Strive.Lenses.Classes"
    , "import Strive.Options"
    , "import Strive.Types"
    ] ++ unlines instances
