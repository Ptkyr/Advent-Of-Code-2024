{-# LANGUAGE TemplateHaskell #-}

import Arrays
import Mayn
import Parsing
import Utils

$(generateMain "15")

type Moves = [Direction]
data Grid = Grid
  { robot :: Coord,
    _grid :: Arr2D Char
  }
  deriving (Show)
type Input = (Grid, Moves)

aocParse :: Parser Input
aocParse = do
  gr <- listArr2D0 <$> some regexdot `endBy` newline
  moves <- newline *> some parseDir <* eof
  let r = fst . head . filter ((== '@') . snd) $ assocs gr
  pure (Grid r gr, moves)

partOne :: Input -> Int
partOne (grid, moves) =
  sum
    . map (gps . fst)
    . filter ((== 'O') . snd)
    . assocs
    . _grid
    $ foldl' move grid moves
 where
  gps (x, y) = x * 100 + y

move :: Grid -> Direction -> Grid
move gr@(Grid r g) dir = matcher (g ! attempt)
 where
  attempt = stepDir dir r
  newRobot = g // [(r, '.'), (attempt, '@')]
  matcher :: Char -> Grid
  matcher '#' = gr
  matcher '.' = Grid attempt newRobot
  matcher _ = case g ! dest of
    '#' -> gr
    '.' -> Grid attempt (newRobot // [(dest, 'O')])
    _ -> error "Unreachable"
   where
    dest = laserTo ((/= 'O') . (g !)) (unitVec dir) r

partTwo :: Input -> Int
partTwo a = 56
