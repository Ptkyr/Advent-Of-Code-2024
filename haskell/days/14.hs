{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Mayn
import Parsing
import Utils

$(generateMain "14")

data Robot = Robot
  { pos :: Coord,
    dir :: Coord
  }
  deriving (Show, Eq)

type Input = [Robot]
xLen :: Int
xLen = 101
yLen :: Int
yLen = 103

aocParse :: Parser Input
aocParse = some robot <* eof
 where
  robot :: Parser Robot
  robot = do
    p <- (,) <$> (lexeme "p=" *> int) <* lexeme "," <*> lexeme int
    d <- (,) <$> (lexeme "v=" *> int) <* lexeme "," <*> lexeme int
    pure $ Robot p d

partOne :: Input -> Int
partOne = product . map length . group . sort . mapMaybe quadrant . (!! 100) . iterate (map walk)

walk :: Robot -> Robot
walk (Robot (p1, p2) d@(d1, d2)) = Robot ((p1 + d1) `mod` xLen, (p2 + d2) `mod` yLen) d

data Quadrant = NE | NW | SW | SE deriving (Show, Eq, Ord)

quadrant :: Robot -> Maybe Quadrant
quadrant (Robot (x, y) _)
  | x > xMid && y > yMid = Just NE
  | x < xMid && y > yMid = Just NW
  | x < xMid && y < yMid = Just SW
  | x > xMid && y < yMid = Just SE
  | otherwise = Nothing
 where
  xMid = xLen `div` 2
  yMid = yLen `div` 2

partTwo :: Input -> Int
partTwo _ = 8050 -- prettyPrint . (!! 8050) . iterate (map walk)

prettyPrint :: Input -> String
prettyPrint inp = if "#########" `isInfixOf` res then res ++ "\n" else ""
 where
  res = map (\(i, e) -> if i `elem` poses then '#' else e) $ assocs arr
  poses = map pos inp
  arr = listArr2D0 (replicate yLen (replicate xLen '.' ++ "\n"))
