{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Control.Exception (assert)
import Mayn
import Parsing
import Utils

$(generateMain "09")

data File = File
  { uuid :: Int,
    copies :: Int
  }
  deriving (Show)
type Files = [File]
type Spaces = [Int]
type Input = (Files, Spaces)

repl :: File -> [Int]
repl (File u c) = replicate c u

decr :: File -> File
decr (File u c) = File u $ c - 1

toFile :: (Int, Int) -> File
toFile (a, b) = File a b

aocParse :: Parser Input
aocParse = do
  (files, spaces) <- unalternate <$> some digit <* eof
  pure (map toFile . assocs $ listArr0 files, spaces)

partOne :: Input -> Int
partOne (files, spaces) = sum $ zipWith (*) [0 ..] decompressed
 where
  decompressed = oneStep Forward files (reverse files) spaces 0

partTwo :: Input -> Int
partTwo a = 56

data Selector = Forward | Backward
oneStep :: Selector -> Files -> Files -> Spaces -> Int -> [Int]
oneStep Forward (f : _) (r : _) [] _ = assert (uuid f == uuid r) $ repl r
oneStep _ _ _ [] _ = []
oneStep Forward (f : files) rev'd spaces idx =
  repl f ++ oneStep Backward files rev'd spaces (idx + copies f)
oneStep Backward files (r : rev'd) (s : spaces) idx
  | s == 0 = oneStep Forward files (r : rev'd) spaces idx
  | copies r == 0 = oneStep Backward files rev'd (s : init spaces) idx
  | otherwise = uuid r : oneStep Backward files (decr r : rev'd) (s - 1 : spaces) (idx + 1)
oneStep _ _ _ _ _ = error "Unreachable"
