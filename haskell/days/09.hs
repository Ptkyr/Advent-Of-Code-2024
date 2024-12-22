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

{-
partOne :: Input -> Int
partOne (files, spaces) = traceShow decompressed $ sum $ zipWith (*) [0 ..] decompressed
 where
  decompressed = concatMap repl solved
  solved = traceShow files $ traceShow spaces $ solve files spaces
-}

partOne :: Input -> Int
partOne (files, spaces) = traceShow decompressed $ sum $ zipWith (*) [0 ..] decompressed
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

fillSpace :: Int -> Files -> Files -> (Files, Files)
fillSpace 0 filled reading = (filled, reading)
fillSpace x filled (r : rs)
  | copies r == 0 = fillSpace x filled rs
  | otherwise = fillSpace (x - 1) newFilled (movedFile : rs)
 where
  movedFile = File (uuid r) (copies r - 1)
  newFilled = filled ++ [File (uuid r) 1]

solve :: Files -> Spaces -> Files
solve files spaces = traceShow ("prefix: " ++ (show $ reverse prefix)) $ traceShow ("mapped: " ++ show filled) $ cursedWeave (reverse prefix) (reverse filled)
 where
  (filled, prefix) = foldl' mapper ([] :: [Files], reverse files) spaces
   where
    mapper :: ([Files], Files) -> Int -> ([Files], Files)
    mapper (fs, rs) x = traceShow ("mapping " ++ show x ++ ": " ++ show mapped) (mapped : fs, newRs)
     where
      (mapped, newRs) = fillSpace x [] rs

cursedWeave :: [a] -> [[a]] -> [a]
cursedWeave [] ys = concat ys
cursedWeave xs [] = xs
cursedWeave (x : xs) (y : ys) = (x : y) ++ cursedWeave xs ys

{-
move :: Int -> (Files, Files) -> (Files, Files)
move 0 (revs, fills) = (revs, fills)
move x (r : rs, fs)
  | numMoved < 0 = fs
  | otherwise = [File (uuid f) $ copies f - 1] : fs
 where
  numMoved = x - copies f
move :: [File] -> Spaces -> [Int]
move files [] = concatMap repl files
move (f : fs) [0 : xs] = repl f ++ move fs xs
move (f : fs) [x : xs] =
 where
  repl :: File -> [Int]
  repl x = replicate (copies x) (uuid x)
-}
