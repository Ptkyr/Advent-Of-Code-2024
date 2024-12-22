{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "07")

data Equation = Equation
  { target :: Int,
    nums :: [Int]
  }
  deriving (Show)

type Binary = Int -> Int -> Int

aocParse :: Parser [Equation]
aocParse = parseEq `endBy` newline <* eof
 where
  parseEq = Equation <$> nat <* lexeme ":" <*> some hnat

partOne :: [Equation] -> Int
partOne = sum . map target . filter (solveEq [(*), (+)])

partTwo :: [Equation] -> Int
partTwo = sum . map target . filter (solveEq [(*), (+), conc])
 where
  conc :: Int -> Int -> Int
  conc = read .: on (++) show

solveEq :: [Binary] -> Equation -> Bool
solveEq _ (Equation _ []) = error "Empty Equation list"
solveEq ops (Equation t (x : xs)) = solve ops t x xs

solve :: [Binary] -> Int -> Int -> [Int] -> Bool
solve _ t acc [] = t == acc
solve ops t acc (x : xs)
  | all (\op -> op acc x > t) ops = False
  | otherwise = any (\op -> solve ops t (op acc x) xs) ops
