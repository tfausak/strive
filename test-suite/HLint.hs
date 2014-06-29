module Main (main) where

import Control.Monad (unless)
import Language.Haskell.HLint (hlint)
import System.Exit (exitFailure)

main :: IO ()
main = hlint arguments >>= flip unless exitFailure . null
  where
    arguments =
        [ "--color"
        , "library"
        , "test-suite"
        ]
