{-# LANGUAGE TemplateHaskell #-}

import Mayn
import Parsing
import Utils

$(generateMain "13")

type Button = Coord
data System = System
  { _a :: Button,
    _b :: Button,
    goal :: Coord
  }
  deriving (Show)
type Input = [System]

aocParse :: Parser Input
aocParse = some system <* eof
 where
  system = do
    a <-
      (,)
        <$> (lexeme "Button A: X+" *> nat)
        <*> (lexeme ", Y+" *> nat)
    b <-
      (,)
        <$> (lexeme "Button B: X+" *> nat)
        <*> (lexeme ", Y+" *> nat)
    g <-
      (,)
        <$> (lexeme "Prize: X=" *> nat)
        <*> (lexeme ", Y=" *> nat)
    pure $ System a b g

partOne :: Input -> Int
partOne = sum . map solve

partTwo :: Input -> Int
partTwo = sum . map (solve . offset)

solve :: System -> Int
solve (System (ax, ay) (bx, by) (x, y))
  | rx `mod` det /= 0 = 0
  | ry `mod` det /= 0 = 0
  | otherwise = 3 * na + nb
 where
  det = ax * by - ay * bx
  rx = x * by - y * bx
  na = rx `div` det
  ry = y * ax - x * ay
  nb = ry `div` det

offset :: System -> System
offset (System a b xy) = System a b (liftT2 (+) xy o)
 where
  o = (big, big)
  big = 10000000000000
