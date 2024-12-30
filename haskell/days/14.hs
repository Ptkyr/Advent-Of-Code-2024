{-# LANGUAGE TemplateHaskell #-}

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
yLen = xLen + 2
modInv :: Int
modInv = (xLen + 1) `div` 2

aocParse :: Parser Input
aocParse = some robot <* eof
 where
  robot :: Parser Robot
  robot = do
    p <- (,) <$> (lexeme "p=" *> int) <* lexeme "," <*> lexeme int
    d <- (,) <$> (lexeme "v=" *> int) <* lexeme "," <*> lexeme int
    pure $ Robot p d

partOne :: Input -> Int
partOne =
  product
    . map length
    . group
    . sort
    . mapMaybe quadrant
    . (!! 100)
    . iterate (map walk)

partTwo :: Input -> Int
partTwo =
  crt
    . foldl' variance ((1000.0, 0), (1000.0, 0))
    . zip [0 ..]
    . take yLen
    . iterate (map walk)
 where
  crt ((_, x), (_, y)) = (modInv * (x * yLen + y * xLen)) `mod` (xLen * yLen)
  variance (x, y) (i, e) = (newvar fst x, newvar snd y)
   where
    newvar f (oldv, old) = if oldv < newv then (oldv, old) else (newv, i)
     where
      newv = sampvar $ map (fromIntegral . f . pos) e

walk :: Robot -> Robot
walk (Robot (p1, p2) d@(d1, d2)) =
  Robot ((p1 + d1) `mod` xLen, (p2 + d2) `mod` yLen) d

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
