{-# LANGUAGE TemplateHaskell #-}

module Mayn where

import Language.Haskell.TH
import System.Clock
import System.IO
import Text.Megaparsec (errorBundlePretty)

generateMain :: String -> Q [Dec]
generateMain day = do
  let file = "inputs/" ++ day ++ ".txt"
  driver <-
    [|
      do
        let f = $(litE $ StringL file)
        parsed <- parseInput aocParse f
        case parsed of
          Left pError -> putStr $ errorBundlePretty pError
          Right input -> do
            print $ partOne input
            print $ partTwo input
      |]
  let typeSig = SigD (mkName "main") (AppT (ConT ''IO) (TupleT 0)) -- IO ()
  let funDec = FunD (mkName "main") [Clause [] (NormalB driver) []]
  return [typeSig, funDec]
