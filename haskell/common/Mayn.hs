{-# LANGUAGE TemplateHaskell #-}

module Mayn where

import Language.Haskell.TH
import System.Clock
import System.IO
import Text.Megaparsec (errorBundlePretty)

timeExec :: Show b => (a -> b) -> a -> IO ()
timeExec f x = do
  start <- getTime Monotonic
  let res = f x
  end <- getTime Monotonic
  let diff = diffTimeSpec end start
  putStrLn $ show res ++ " in " ++ show (toNanoSecs diff) ++ "(ns)"

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
            timeExec partOne input
            timeExec partTwo input
      |]
  let typeSig = SigD (mkName "main") (AppT (ConT ''IO) (TupleT 0)) -- IO ()
  let funDec = FunD (mkName "main") [Clause [] (NormalB driver) []]
  return [typeSig, funDec]
